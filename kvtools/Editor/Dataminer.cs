using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Reflection;
using UnityEditor;
using UnityEngine;
using VRC.Udon.Graph;
using VRC.Udon.Common.Interfaces;
using VRC.Udon.Editor;
using VRC.Udon.EditorBindings;
using UdonSharp.Compiler.Udon;

namespace KDCVRCTools {
	public static class Dataminer {
		private static readonly BindingFlags lessThanSaneBindingFlags = BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public;
		// Fun fact: This used to be called 'DatamineZVZ'.
		[MenuItem("VRChat SDK/KDCVRCTools/Datamine")]
		public static void ExtractData() {
			// prepare...
			var compilerUdonInterface = Type.GetType("UdonSharp.Compiler.Udon.CompilerUdonInterface, UdonSharp.Editor");

			var cacheInit = compilerUdonInterface.GetMethod("CacheInit", lessThanSaneBindingFlags);
			cacheInit.Invoke(null, new object[] {});

			UdonEditorInterface editorInterfaceInstance = (UdonEditorInterface) compilerUdonInterface.GetField("_editorInterfaceInstance", lessThanSaneBindingFlags).GetValue(null);
			Dictionary<string, string> builtinEventLookup = (Dictionary<string, string>) compilerUdonInterface.GetField("_builtinEventLookup", lessThanSaneBindingFlags).GetValue(null);
			Dictionary<string, ImmutableArray<(string, Type)>> builtinEventArgumentsLookup = (Dictionary<string, ImmutableArray<(string, Type)>>) compilerUdonInterface.GetField("_builtinEventArgumentsLookup", lessThanSaneBindingFlags).GetValue(null);
			var getUdonTypeName = compilerUdonInterface.GetMethod("GetUdonTypeName", lessThanSaneBindingFlags, null, new Type[] {typeof(Type)}, null);

			// can now begin
			List<string> total = new();
			HashSet<Type> types = new();
			foreach (UdonNodeDefinition und in editorInterfaceInstance.GetNodeDefinitions()) {
				if (!und.fullName.Contains("."))
					continue;
				total.Add("EXTERN");
				total.Add(und.fullName);
				total.Add(GetUdonTypeName(getUdonTypeName, und.type));
				DiscoverTypes(types, und.type);
				total.Add(und.parameters.Count.ToString());
				foreach (UdonNodeParameter par in und.parameters) {
					total.Add(par.name);
					total.Add(GetUdonTypeName(getUdonTypeName, par.type));
					DiscoverTypes(types, par.type);
					total.Add(par.parameterType.ToString());
				}
			}
			foreach (KeyValuePair<string, string> kvp in builtinEventLookup) {
				total.Add("EVENT");
				total.Add(kvp.Key);
				total.Add(kvp.Value);
				var args = builtinEventArgumentsLookup[kvp.Key];
				total.Add(args.Length.ToString());
				foreach ((string, Type) tuple in args)
				{
					total.Add(tuple.Item1);
					DiscoverTypes(types, tuple.Item2);
					total.Add(GetUdonTypeName(getUdonTypeName, tuple.Item2));
				}
			}
			foreach (Type t in types) {
				total.Add("TYPE");
				string typeUdonName = GetUdonTypeName(getUdonTypeName, t);
				total.Add(typeUdonName);
				total.Add(t.FullName);
				if (t.BaseType != null) {
					total.Add(GetUdonTypeName(getUdonTypeName, t.BaseType));
				} else {
					total.Add("");
				}
				var interfaces = t.GetInterfaces();
				total.Add(interfaces.Length.ToString());
				foreach (Type it in interfaces)
					total.Add(GetUdonTypeName(getUdonTypeName, it));
				// if enum, add values
				if (t.BaseType == typeof(System.Enum)) {
					foreach (object v in Enum.GetValues(t)) {
						total.Add("EVAL");
						total.Add(typeUdonName);
						total.Add(Enum.GetName(t, v));
						total.Add(Convert.ToInt64(v).ToString());
					}
				}
			}
			// add events that aren't exposed
			total.Add("EVENT");
			total.Add("OnAudioFilterRead");
			total.Add("_onAudioFilterRead");
			total.Add("2");
			total.Add("onAudioFilterReadData");
			total.Add("SystemSingleArray");
			total.Add("onAudioFilterReadChannels");
			total.Add("SystemInt32");
			// done!
			total.Add("END");
			System.IO.File.WriteAllText(Application.dataPath + "/datamine.txt", string.Join("\n", total));
		}

		public static string GetUdonTypeName(MethodInfo method, Type type) {
			return (string) method.Invoke(null, new object[] {type});
		}

		private static void DiscoverTypes(HashSet<Type> hs, Type t) {
			if (hs.Add(t))
				return;
			if (t.BaseType != null)
				DiscoverTypes(hs, t.BaseType);
			foreach (Type it in t.GetInterfaces())
				DiscoverTypes(hs, it);
		}
	}
}
