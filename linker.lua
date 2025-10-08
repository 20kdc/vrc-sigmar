local board = {
	"     * * * * * *",
	"    * * * * * * *",
	"   * * * * * * * *",
	"  * * * * * * * * *",
	" * * * * * * * * * *",
	"* * * * * * * * * * *",
	" * * * * * * * * * *",
	"  * * * * * * * * *",
	"   * * * * * * * *",
	"    * * * * * * *",
	"     * * * * * *"
}
local points = {}
local ptExist = {}
local function ptExists(x, y)
	return ptExist[tostring(x) .. "_" .. tostring(y)]
end
for y, v in ipairs(board) do
	for x = 1, #v do
		if v:sub(x, x) == "*" then
			ptExist[tostring(x) .. "_" .. tostring(y)] = #points
			table.insert(points, {x = x, y = y})
		end
	end
end
local tableTotal = ""
for k, v in ipairs(points) do
	local function tryWr(ox1, oy1)
		local p1 = ptExists(v.x + ox1, v.y + oy1) or -1
		tableTotal = tableTotal .. tostring(p1) .. " "
	end
	tryWr(-1, -1)
	tryWr(1, -1)
	tryWr(2, 0)
	tryWr(1, 1)
	tryWr(-1, 1)
	tryWr(-2, 0)
end
print(tableTotal)
