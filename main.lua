---@class SpriteCodec
local SCMod = {}
SCMod.Version = '2.9.0'
local path=require('debug').getinfo(1,'S').source:match('^@(.*)main.lua$')
-- local Dots = {}
local CacheIndex = {}
local Index2Compressed = {}
local Cache = {}
local MaxCacheSize = 128
local ScrollCacheId = 0
local DOT = nil

---------------------------------------------------------------------------------------

local function EncodeDot(r, g, b, a)
    if a==0 then
        r, g, b = 0, 0, 0
    end
    local pack = {[1] = r or 1, [2] = g or 1, [3] = b or 1, [4] = a or 1}
    local code = 0
    for k,v in ipairs(pack)do
        pack[k] = math.floor(v*255+.5)
        code = code<<8 | pack[k]&0xFF
    end
    return string.pack('>I4', code)
end

local function DecodeDot(code)
    local success,decode = pcall(string.unpack,'>I4',code)
    if not success then
        error('Invalid code', 2)
    end
    local pack = {}
    for i=4,1,-1 do
        pack[i] = (decode & 0xFF)/255
        decode = decode>>8
    end
    return table.unpack(pack)
end

local function Dot1UnderDot2(rlow,glow,blow,alow,rhigh,ghigh,bhigh,ahigh)
    if rlow and glow and blow and alow and rhigh and ghigh and bhigh and ahigh then
        if alow == 0 then
            return rhigh,ghigh,bhigh,ahigh
        elseif ahigh == 0 then
            return rlow,glow,blow,alow
        end
        local r,g,b,a
        a=ahigh+alow*(1-ahigh)
        r=(rhigh*ahigh+rlow*alow*(1-ahigh))/a
        g=(ghigh*ahigh+glow*alow*(1-ahigh))/a
        b=(bhigh*ahigh+blow*alow*(1-ahigh))/a
        return r,g,b,a
    else
        error('Invalid color values', 2)
    end
end

local function NewDot(r,g,b,a)
    local dot=Sprite()
    dot:Load(path..'/gfx/dot.anm2', true)
    dot:SetFrame('dot', 0)
    dot.Color = Color(r or 1, g or 1, b or 1, a or 1)
    return dot
end

local function GetDot(r,g,b,a)
    if not (DOT and DOT:IsLoaded()) then
        DOT = NewDot(1,1,1,1)
    end
    DOT.Color = Color(r or 1, g or 1, b or 1, a or 1)
    return DOT
    -- local code = EncodeDot(r,g,b,a)
    -- if not Dots[code] then
    --     Dots[code] = NewDot(r,g,b,a)
    -- end
    -- return Dots[code]
end

local function RLECompress(str)
    if type(str) ~= 'string' then
        error('Argument isn\'t a string', 2)
    end
    local result = {}
    local len = #str
    local i = 1
    while i <= len do
        local count = 1
        local c = str:sub(i, i)
        while i + count <= len and str:sub(i + count, i + count) == c and count < 255 do
            count = count + 1
        end
        table.insert(result, string.char(count))
        table.insert(result, c)
        i = i + count
    end
    return table.concat(result)
end

local function RLEDecompress(rle)
    if type(rle) ~= 'string' then
        error('Argument isn\'t a string', 2)
    end
    local result = {}
    local i = 1
    local len = #rle
    while i < len do
        local count = string.byte(rle:sub(i, i))
        local c = rle:sub(i + 1, i + 1)
        table.insert(result, string.rep(c, count))
        i = i + 2
    end
    return table.concat(result)
end

-- 预测编码压缩
local function PredictiveCompress(pixelData)
    if type(pixelData) ~= 'string' then
        error('Argument isn\'t a string', 2)
    end
    -- 对像素数据进行预测编码
    local pixelBytes = {}
    for i = 1, #pixelData do
        pixelBytes[i] = pixelData:byte(i)
    end
    
    -- 使用前一个像素的相同通道作为预测值
    local compressedPixels = {}
    local prevR, prevG, prevB, prevA = 0, 0, 0, 0
    
    for i = 1, #pixelBytes, 4 do
        -- 处理每个像素的RGBA四个通道
        local r = pixelBytes[i]
        local g = pixelBytes[i+1]
        local b = pixelBytes[i+2]
        local a = pixelBytes[i+3]
        
        -- 计算差值（使用前一个像素的相同通道作为预测）
        local diffR = (r - prevR + 256) % 256
        local diffG = (g - prevG + 256) % 256
        local diffB = (b - prevB + 256) % 256
        local diffA = (a - prevA + 256) % 256
        
        -- 更新预测值
        prevR, prevG, prevB, prevA = r, g, b, a
        
        -- 编码差值
        table.insert(compressedPixels, string.char(diffR))
        table.insert(compressedPixels, string.char(diffG))
        table.insert(compressedPixels, string.char(diffB))
        table.insert(compressedPixels, string.char(diffA))
    end
    
    return table.concat(compressedPixels)
end

-- 预测编码解压缩
local function PredictiveDecompress(compressedPixels)
    if type(compressedPixels) ~= 'string' then
        error('Argument isn\'t a string', 2)
    end
    -- 重建像素数据
    local pixelBytes = {}
    local prevR, prevG, prevB, prevA = 0, 0, 0, 0
    local pixelIndex = 1
    
    for i = 1, #compressedPixels, 4 do
        local diffR = compressedPixels:byte(i)
        local diffG = compressedPixels:byte(i+1)
        local diffB = compressedPixels:byte(i+2)
        local diffA = compressedPixels:byte(i+3)
        
        -- 重建原始值
        local r = (prevR + diffR) % 256
        local g = (prevG + diffG) % 256
        local b = (prevB + diffB) % 256
        local a = (prevA + diffA) % 256
        
        -- 更新预测值
        prevR, prevG, prevB, prevA = r, g, b, a
        
        -- 存储重建的像素数据
        pixelBytes[pixelIndex] = string.char(r)
        pixelBytes[pixelIndex+1] = string.char(g)
        pixelBytes[pixelIndex+2] = string.char(b)
        pixelBytes[pixelIndex+3] = string.char(a)
        pixelIndex = pixelIndex + 4
    end
    
    return table.concat(pixelBytes)
end

local function Compress(header, map, code)
    if type(header) ~= 'string' or type(map) ~= 'string' or type(code) ~= 'string' then
        error('Arguments must be strings', 2)
    end
    local mapRLE = RLECompress(map)
    local mapLen = string.pack('>I4', #mapRLE)
    return header .. mapLen .. mapRLE .. PredictiveCompress(code)
end

local function Decompress(compressed)
    if type(compressed) ~= 'string' then
        error('Argument isn\'t a string', 2)
    end
    if CacheIndex[compressed] then
        return Cache[CacheIndex[compressed]]
    else
        local header = compressed:sub(1, 4)
        local success,mapLen = pcall(string.unpack, '>I4', compressed:sub(5, 8))
        if not success then
            error('Invalid compressed code', 2)
        end
        local mapRLE = compressed:sub(9, 8 + mapLen)
        local map = RLEDecompress(mapRLE)
        local code = compressed:sub(9 + mapLen)
        local decompressed = header .. map .. PredictiveDecompress(code)

        ScrollCacheId = ScrollCacheId % MaxCacheSize + 1

        local id2co = Index2Compressed[ScrollCacheId]
        if id2co then
            CacheIndex[id2co] = nil
        end
        
        CacheIndex[compressed] = ScrollCacheId
        Index2Compressed[ScrollCacheId] = compressed
        Cache[ScrollCacheId] = decompressed
        return decompressed
    end
end

local function GenerateCode(arm, leg, matrix)
    if type(arm) ~= 'number' or type(leg) ~= 'number' or type(matrix) ~= 'table' then
        error('Invalid arguments', 2)
    end
    local header = string.pack('>HH', arm, leg)
    local map = ''
    local byte, cycle = 0, 0
    local code = ''
    for y = -leg, leg do
        for x = -arm, arm do
            local k = matrix[x] and matrix[x][y] or {r=0, g=0, b=0, a=0}
            if k.a > 0 then
                byte = byte | 1 << cycle
                code = code .. EncodeDot(k.r, k.g, k.b, k.a)
            end
            cycle = cycle + 1
            if cycle >= 8 then
                map = map .. string.char(byte)
                byte, cycle = 0, 0
            end
        end
    end
    if cycle > 0 then
        map = map .. string.char(byte)
    end
    return {header, map, code}
end
-------------------------------------------------------------------------------------
local generatedEmptyCode_raw = GenerateCode(0,0,{})
SCMod.RawEmptyCode = table.concat(generatedEmptyCode_raw)
SCMod.EmptyCode = Compress(table.unpack(generatedEmptyCode_raw))
-------------------------------------------------------------------------------------
function SCMod:MemberIter()
    local keys,values,id = {},{},0
    for k,v in pairs(SCMod)do
        id = id + 1
        keys[id]=k
        values[id]=v
    end
    local i=0
    return function()
        i = i + 1
        if i <= id then
            return keys[i],values[i]
        end
    end
end
-------------------------------------------------------------------------------------
---@param sprite Sprite
---@param layer number
---@param arm number|nil 图像中心到水平边缘的最大距离，默认64
---@param leg number|nil 图像中心到垂直边缘的最大距离，默认64
---@param rawOutput boolean|nil 是否输出未压缩的编码表示，默认false
---@return string --code: 将指定图层的Sprite(当前帧)编码为压缩后的字符串表示
---@nodiscard
function SCMod:EncodeSpriteLayer(sprite, layer, arm, leg, rawOutput)
    if type(layer)~='number' or layer < 0 then
        error('Invalid layer', 2)
    end
    local success, layerCount = pcall(sprite.GetLayerCount, sprite)
    if not success then
        error('Invalid sprite', 2)
    end
    if layer >= layerCount then
        error('Layer out of range', 2)
    end
    local tmp = {}
    arm = math.ceil(arm or 64)
    leg = math.ceil(leg or 64)
    local left, right, top, bottom = math.huge, -math.huge, math.huge, -math.huge
    for x = -arm, arm do
        tmp[x] = {}
        for y = -leg, leg do
            local k = sprite:GetTexel(Vector(x, y), Vector.Zero, 1, layer)
            if k.Alpha > 0 then
                tmp[x][y] = {r=k.Red, g=k.Green, b=k.Blue, a=k.Alpha}
                if x < left then left = x end
                if x > right then right = x end
                if y < top then top = y end
                if y > bottom then bottom = y end
            end
        end
    end
    if left > right then
        left, right = 0, 0
    end
    if top > bottom then
        top, bottom = 0, 0
    end
    arm = math.max(math.abs(left), math.abs(right))
    leg = math.max(math.abs(top), math.abs(bottom))

    local generatedCode = GenerateCode(arm, leg, tmp)
    if rawOutput then
        return table.concat(generatedCode)
    end
    return Compress(table.unpack(generatedCode))
end

---@param code string 编码表示的图像数据
---@param renderPos Vector 表示渲染位置的二维向量
---@param filter fun(x:number, y:number, r:number, g:number, b:number, a:number):(x_new:number,y_new:number, r_new:number, g_new:number, b_new:number, a_new:number)|nil 表示在渲染时对像素进行过滤处理的函数，接受像素的坐标和RGBA值作为参数，返回修改后的坐标和RGBA值,如果为nil则不进行过滤
---@param rawInput boolean|nil 表示输入的编码是否为未压缩格式，默认false
function SCMod:RenderCode(code, renderPos, filter, rawInput)
    if type(code)~='string' then
        error('Code isn\'t a string', 2)
    end
    if not (renderPos and renderPos.X and renderPos.Y) then
        error('Invalid render position', 2)
    end
    filter = type(filter) == 'function' and filter or nil
    if not rawInput then
        local success, decompressed = pcall(Decompress, code)
        if not success then
            error(decompressed, 2)
        end
        code = decompressed
    end
    local success, arm, leg = pcall(string.unpack, '>HH', code:sub(1, 4))
    if not success then
        error('Invalid code', 2)
    end
    local mapSize = math.ceil((2 * arm + 1) * (2 * leg + 1) / 8)
    local map = code:sub(5, 4 + mapSize)
    code = code:sub(5 + mapSize)
    local filterChecked = false
    for c=1, mapSize do
        local byte = map:byte(c)
        for i=0,7 do
            if byte&1>0 then
                local x = ((c - 1) * 8 + i) % (2 * arm + 1) - arm
                local y = math.floor(((c - 1) * 8 + i) / (2 * arm + 1)) - leg
                local coDot = code:sub(1, 4)
                code = code:sub(5)
                local r, g, b, a = DecodeDot(coDot)
                local px,py,pr,pg,pb,pa = x,y,r,g,b,a
                if filter and not filterChecked then
                    px, py, pr, pg, pb, pa = filter(x, y, r, g, b, a)
                    local retVal = {px, py, pr, pg, pb, pa}
                    assert(#retVal==6, 'Filter function must return six numbers')
                    for k,v in ipairs(retVal) do
                        assert(type(v)=='number', 'Filter function return value #'..k..' is not a number')
                    end
                    filterChecked = true
                end
                if pa > 0 then
                    local dot = GetDot(pr, pg, pb, pa)
                    dot:Render(Vector(renderPos.X + px, renderPos.Y + py))
                end
            end
            byte = byte >> 1
        end
    end
end

local Base64Index = {}
local Base64Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
for i = 1, #Base64Chars do
    Base64Index[i - 1] = Base64Chars:sub(i, i)
end

---@param code string 编码表示的图像数据
---@return string --sharecode: 可读的Base64字符串
---@nodiscard
function SCMod:Share(code)
    if type(code) ~= 'string' then
        error('Argument isn\'t a string', 2)
    end
    local sharecode = ''
    local len = #code
    local i = 1
    while i <= len do
        local b1 = code:byte(i) or 0
        local b2 = code:byte(i + 1) or 0
        local b3 = code:byte(i + 2) or 0

        local c1 = b1 >> 2
        local c2 = ((b1 & 0x3) << 4) | (b2 >> 4)
        local c3 = ((b2 & 0xF) << 2) | (b3 >> 6)
        local c4 = b3 & 0x3F

        sharecode = sharecode .. Base64Index[c1] .. Base64Index[c2]
        if i + 1 <= len then
            sharecode = sharecode .. Base64Index[c3]
        else
            sharecode = sharecode .. '='
        end
        if i + 2 <= len then
            sharecode = sharecode .. Base64Index[c4]
        else
            sharecode = sharecode .. '='
        end

        i = i + 3
    end
    return sharecode
end

local Base64Find = {}
for i = 0, 63 do
    Base64Find[Base64Index[i]] = i
end

---@param sharecode string Base64编码的图像数据
---@return string --code: 解码后的图像数据
---@nodiscard
function SCMod:Receive(sharecode)
    if type(sharecode) ~= 'string' then
        error('Argument isn\'t a string', 2)
    end
    local code = ''
    local len = #sharecode
    local i = 1
    while i <= len do
        local c1 = Base64Find[sharecode:sub(i, i)] or 0
        local c2 = Base64Find[sharecode:sub(i + 1, i + 1)] or 0
        local c3 = sharecode:sub(i + 2, i + 2)
        local c4 = sharecode:sub(i + 3, i + 3)

        c3 = c3 == '=' and nil or Base64Find[c3]
        c4 = c4 == '=' and nil or Base64Find[c4]

        local b1 = (c1 << 2) | (c2 >> 4)
        local b2 = ((c2 & 0xF) << 4) | ((c3 or 0) >> 2)
        local b3 = (((c3 or 0) & 0x3) << 6) | (c4 or 0)

        code = code .. string.char(b1)
        if c3 ~= nil then code = code .. string.char(b2) end
        if c4 ~= nil then code = code .. string.char(b3) end

        i = i + 4
    end
    return code
end

---@param path2png string PNG图像的文件路径
---@param width number|nil 图像的宽度，默认(最大)128
---@param height number|nil 图像的高度，默认(最大)128
---@param rawOutput boolean|nil 是否返回未压缩的原始编码，默认false
---@return string --code: 将指定路径的PNG图像编码为压缩后的字符串表示
---@nodiscard
function SCMod:EncodePNG(path2png, width, height, rawOutput)
    if type(path2png) ~= 'string' then
        error('Path isn\'t a string', 2)
    end
    width = math.min(128, math.ceil(width or 128))
    height = math.min(128, math.ceil(height or 128))

    -- 保证 2*arm+1 == width 且 2*leg+1 == height
    local arm = math.floor((width - 1) / 2)
    local leg = math.floor((height - 1) / 2)

    local png = Sprite()
    png:Load(path..'/gfx/transparent.anm2')
    png:ReplaceSpritesheet(0, path2png)
    png:LoadGraphics()
    png:SetFrame('transparent', 0)

    local left, right, top, bottom = math.huge, -math.huge, math.huge, -math.huge
    local tmp = {}

    -- 遍历严格对齐到声明范围 [-arm..arm] × [-leg..leg]
    for x = -arm, arm do
        tmp[x] = {}
        for y = -leg, leg do
            local sx = x + arm          -- 0 .. width-1
            local sy = y + leg          -- 0 .. height-1
            local k = png:GetTexel(Vector(sx, sy), Vector.Zero, 1, 0)
            if k.Alpha > 0 then
                tmp[x][y] = { r = k.Red, g = k.Green, b = k.Blue, a = k.Alpha }
                if x < left then left = x end
                if x > right then right = x end
                if y < top then top = y end
                if y > bottom then bottom = y end
            end
        end
    end

    -- 无像素时安全回退
    if left > right then left, right = 0, 0 end
    if top > bottom then top, bottom = 0, 0 end

    -- 以实际像素边界收紧范围
    arm = math.max(math.abs(left), math.abs(right))
    leg = math.max(math.abs(top), math.abs(bottom))

    local generatedCode = GenerateCode(arm, leg, tmp)
    if rawOutput then
        return table.concat(generatedCode)
    end
    return Compress(table.unpack(generatedCode))
end

---@param code string 编码表示的图像数据
---@param modifier fun(x:number, y:number, r:number, g:number, b:number, a:number):(x_new:number,y_new:number, r_new:number, g_new:number, b_new:number, a_new:number)|nil 表示对像素进行修改的函数，接受像素的坐标和RGBA值作为参数，返回修改后的坐标和RGBA值,如果为nil则不进行修改
---@param rawInput boolean|nil 表示输入的编码是否为未压缩格式，默认false
---@param rawOutput boolean|nil 表示输出的编码是否为未压缩格式，默认false
---@return string --code: 修改后的图像数据编码
---@nodiscard
function SCMod:ReshapeCode(code, modifier, rawInput, rawOutput)
    if type(code)~='string' then
        error('Code isn\'t a string', 2)
    end
    modifier = type(modifier) == 'function' and modifier or nil
    local original_code = code
    local success,decompressed
    if not rawInput then
        success, decompressed = pcall(Decompress, code)
        if not success then
            error(decompressed, 2)
        end
        code = decompressed
    end
    local header = code:sub(1, 4)
    local success, arm, leg = pcall(string.unpack, '>HH', header)
    if not success then
        error('Invalid code', 2)
    end
    local mapSize = math.ceil((2 * arm + 1) * (2 * leg + 1) / 8)
    local map = code:sub(5, 4 + mapSize)
    code = code:sub(5 + mapSize)
    local modifierChecked = false
    if modifier then
        local left, right, top, bottom = math.huge, -math.huge, math.huge, -math.huge
        local tmp={}
        local transparentDot = EncodeDot(0,0,0,0)
        for c=1, mapSize do
            local byte = map:byte(c)
            for i=0,7 do
                local x = ((c - 1) * 8 + i) % (2 * arm + 1) - arm
                local y = math.floor(((c - 1) * 8 + i) / (2 * arm + 1)) - leg
    
                local coDot = transparentDot
                if byte&1>0 then
                    coDot = code:sub(1, 4)
                    code = code:sub(5)
                end
                local r, g, b, a = DecodeDot(coDot)
                local px, py, pr, pg, pb, pa = modifier(x, y, r, g, b, a)
                if not modifierChecked then
                    local retVal = {px, py, pr, pg, pb, pa}
                    assert(#retVal==6, 'Modifier function must return six numbers')
                    for k,v in ipairs(retVal) do
                        assert(type(v)=='number', 'Modifier function return value #'..k..' is not a number')
                    end
                    modifierChecked = true
                end
                if pa > 0 then
                    tmp[px] = tmp[px] or {}
                    tmp[px][py] = {r=pr, g=pg, b=pb, a=pa}
                    if px < left then left = px end
                    if px > right then right = px end
                    if py < top then top = py end
                    if py > bottom then bottom = py end
                end
    
                byte = byte >> 1
            end
        end
        if left > right then
            left, right = 0, 0
        end
        if top > bottom then
            top, bottom = 0, 0
        end
        arm = math.ceil(math.max(math.abs(left), math.abs(right)))
        leg = math.ceil(math.max(math.abs(top), math.abs(bottom)))

        local generatedCode = GenerateCode(arm, leg, tmp)
        if rawOutput then
            return table.concat(generatedCode)
        end
        return Compress(table.unpack(generatedCode))
    elseif rawOutput then
        if rawInput then
            return original_code
        end
        return decompressed
    elseif rawInput then
        return Compress(header, map, code)
    else
        return original_code
    end
end

---@param code1 string 编码表示的图像数据1
---@param code2 string 编码表示的图像数据2
---@param mode fun(rlow:number, glow:number, blow:number, alow:number, rhigh:number, ghigh:number, bhigh:number, ahigh:number):(r:number, g:number, b:number, a:number)|nil 表示对两个像素进行混合的函数，接受两个像素的RGBA值作为参数，返回混合后的RGBA值,如果为nil则使用默认的"2层覆盖1层"模式
---@param rawInput boolean|nil 表示输入的编码是否为未压缩格式，默认false
---@param rawOutput boolean|nil 表示输出的编码是否为未压缩格式，默认false
---@return string --code: 混合后的图像数据编码
---@nodiscard
function SCMod:MixCode(code1, code2, mode, rawInput, rawOutput)
    if type(code1)~='string' or type(code2)~='string' then
        error('Codes aren\'t strings', 2)
    end
    mode = type(mode) == 'function' and mode or Dot1UnderDot2
    if not rawInput then
        local success, decompressed1, decompressed2
        success, decompressed1 = pcall(Decompress, code1)
        if not success then
            error('Code1 error: '..decompressed1, 2)
        end
        success, decompressed2 = pcall(Decompress, code2)
        if not success then
            error('Code2 error: '..decompressed2, 2)
        end
        code1, code2 = decompressed1, decompressed2
    end
    local success1, arm1, leg1 = pcall(string.unpack, '>HH', code1:sub(1, 4))
    if not success1 then
        error('Invalid code1', 2)
    end
    local success2, arm2, leg2 = pcall(string.unpack, '>HH', code2:sub(1, 4))
    if not success2 then
        error('Invalid code2', 2)
    end
    local arm = math.max(arm1, arm2)
    local leg = math.max(leg1, leg2)
    local map1Size = math.ceil((2 * arm1 + 1) * (2 * leg1 + 1) / 8)
    local map2Size = math.ceil((2 * arm2 + 1) * (2 * leg2 + 1) / 8)
    local map1 = code1:sub(5, 4 + map1Size)
    local map2 = code2:sub(5, 4 + map2Size)
    code1 = code1:sub(5 + map1Size)
    code2 = code2:sub(5 + map2Size)
    
    local tmp1={}
    local tmp2={}
    local transparentDot = EncodeDot(0,0,0,0)
    for c=1, map1Size do
        local byte = map1:byte(c)
        for i=0,7 do
            if byte&1>0 then
                local x = ((c - 1) * 8 + i) % (2 * arm1 + 1) - arm1
                local y = math.floor(((c - 1) * 8 + i) / (2 * arm1 + 1)) - leg1

                local coDot = transparentDot
                if byte&1>0 then
                    coDot = code1:sub(1, 4)
                    code1 = code1:sub(5)
                end
                local r, g, b, a = DecodeDot(coDot)
                tmp1[x] = tmp1[x] or {}
                tmp1[x][y] = {r=r, g=g, b=b, a=a}
            end
            byte = byte >> 1
        end
    end
    for c=1, map2Size do
        local byte = map2:byte(c)
        for i=0,7 do
            if byte&1>0 then
                local x = ((c - 1) * 8 + i) % (2 * arm2 + 1) - arm2
                local y = math.floor(((c - 1) * 8 + i) / (2 * arm2 + 1)) - leg2

                local coDot = transparentDot
                if byte&1>0 then
                    coDot = code2:sub(1, 4)
                    code2 = code2:sub(5)
                end
                local r, g, b, a = DecodeDot(coDot)
                tmp2[x] = tmp2[x] or {}
                tmp2[x][y] = {r=r, g=g, b=b, a=a}
            end
            byte = byte >> 1
        end
    end

    local tmp = {}
    local modeChecked = false
    for x = -arm, arm do
        tmp[x] = {}
        for y = -leg, leg do
            local p = tmp1[x] and tmp1[x][y] or {r=0, g=0, b=0, a=0}
            local q = tmp2[x] and tmp2[x][y] or {r=0, g=0, b=0, a=0}
            local r, g, b, a = mode(p.r, p.g, p.b, p.a, q.r, q.g, q.b, q.a)
            if not modeChecked then
                local retVal = {r, g, b, a}
                assert(#retVal==4, 'Mode function must return 4 numbers')
                for k,v in ipairs(retVal) do
                    assert(type(v)=='number', 'Mode function return value #'..k..' is not a number')
                end
                modeChecked = true
            end
            tmp[x][y] = a>0 and {r=r, g=g, b=b, a=a} or nil
        end
    end
    local generatedCode = GenerateCode(arm, leg, tmp)
    if rawOutput then
        return table.concat(generatedCode)
    end
    return Compress(table.unpack(generatedCode))
end

---@param sprite Sprite
---@param arm number|nil 图像中心到水平边缘的最大距离，默认与EncodeSpriteLayer相同
---@param leg number|nil 图像中心到垂直边缘的最大距离，默认与EncodeSpriteLayer相同
---@param rawOutput boolean|nil 是否输出未压缩的编码表示，默认false
---@return string --code: 将指定Sprite的所有图层(当前帧)编码为压缩后的字符串表示
---@nodiscard
function SCMod:EncodeSprite(sprite, arm, leg, rawOutput)
    local success, lastCode = pcall(self.EncodeSpriteLayer, self, sprite, 0, arm, leg, true)
    if not success then
        error(lastCode, 2)
    end
    for i=1, sprite:GetLayerCount()-1 do
        local newCode = self:EncodeSpriteLayer(sprite, i, arm, leg, true)
        lastCode = self:MixCode(lastCode, newCode, Dot1UnderDot2, true, true)
    end
    if rawOutput then
        return lastCode
    end
    local header = lastCode:sub(1, 4)
    success, arm, leg = pcall(string.unpack, '>HH', header)
    if not success then
        error('Invalid header', 1)
    end
    local mapSize = math.ceil((2 * arm + 1) * (2 * leg + 1) / 8)
    local map = lastCode:sub(5, 4 + mapSize)
    local code = lastCode:sub(5 + mapSize)
    return Compress(header, map, code)
end

---@module 'qrencode'
local qrencode = require('qrencode')

---@param content string 要编码为二维码的文本内容
---@param rawOutput boolean|nil 是否输出未压缩的编码表示，默认false
---@return string --code: 将指定内容编码为二维码的压缩后的字符串表示
---@nodiscard
function SCMod:QRCode(content, rawOutput)
    -- 生成二维码
    local ok, qr_matrix = qrencode.qrcode(content)
    if not ok then
        error('Failed to generate QR code', 2)
    end
    
    -- 获取原始二维码尺寸
    local original_size = #qr_matrix
    
    -- 二维码实际内容区域（去掉四周的空白边界）
    -- 通常二维码四周有4个模块的空白边界，但实际可能因版本而异
    -- 这里我们假设去掉最外层的空白
    local content_size = original_size
    local start_index = 1
    local end_index = original_size
    
    -- 计算中心坐标的偏移量（基于实际内容区域）
    local center_offset = math.floor(content_size / 2)
    
    -- 创建新的矩阵，坐标以中心为原点
    local centered_matrix = {}
    
    -- 填充新矩阵，只包含实际内容区域
    for x = -center_offset, center_offset do
        centered_matrix[x] = {}
        for y = -center_offset, center_offset do
            -- 计算原始矩阵中的索引（跳过边界）
            local orig_x = x + center_offset + start_index
            local orig_y = y + center_offset + start_index
            
            -- 确保索引在有效范围内
            if orig_x >= start_index and orig_x <= end_index and 
               orig_y >= start_index and orig_y <= end_index then
                -- 根据二维码值设置颜色
                if qr_matrix[orig_x][orig_y] > 0 then
                    centered_matrix[x][y] = {r = 0, g = 0, b = 0, a = 1}  -- 黑色
                else
                    centered_matrix[x][y] = {r = 1, g = 1, b = 1, a = 1}  -- 白色
                end
            else
                centered_matrix[x][y] = {r = 1, g = 1, b = 1, a = 1}  -- 边界外设为白色
            end
        end
    end
    
    local generatedCode = GenerateCode(center_offset, center_offset, centered_matrix)
    if rawOutput then
        return table.concat(generatedCode)
    end
    return Compress(table.unpack(generatedCode))
end

---@param code string 编码表示的图像数据
---@return Vector --range: 返回表示图像范围的二维向量，X分量为水平范围(中心到边缘的最大距离)，Y分量为垂直范围(中心到边缘的最大距离)
---@nodiscard
function SCMod:GetCodeRange(code)
    local success, arm, leg = pcall(string.unpack, '>HH', code:sub(1, 4))
    if not success then
        error('Invalid code', 2)
    end
    return Vector(arm, leg)
end

---@param code string 编码表示的图像数据
---@return number --pixelNum: 返回图像中非透明像素的数量
---@nodiscard
function SCMod:GetCodePixelNum(code,rawInput)
    local success, arm, leg = pcall(string.unpack, '>HH', code:sub(1, 4))
    if not success then
        error('Invalid code', 2)
    end
    local mapSize,map = math.ceil((2 * arm + 1) * (2 * leg + 1) / 8)
    local pixelNum=0
    if rawInput then
        map = code:sub(5, 4 + mapSize)
    else
        local success, mapLen = pcall(string.unpack, '>I4', code:sub(5, 8))
        if not success then
            error('Invalid code', 2)
        end
        success, map = pcall(RLEDecompress, code:sub(9, 8 + mapLen))
        if not success then
            error('Invalid code', 2)
        end
    end
    for c=1, mapSize do
        local byte = map:byte(c)
        for _=0,7 do
            if byte&1>0 then
                pixelNum=pixelNum+1
            end
            byte = byte >> 1
        end
    end
    return pixelNum
end

---@param code string 编码表示的图像数据
---@param rawInput boolean|nil 表示输入的编码是否为未压缩格式，默认false
---@return Vector --center: 返回图像中心的二维向量坐标
---@nodiscard
function SCMod:GetCodeCenter(code,rawInput)
    local success, arm, leg = pcall(string.unpack, '>HH', code:sub(1, 4))
    if not success then
        error('Invalid code', 2)
    end
    local mapSize,map = math.ceil((2 * arm + 1) * (2 * leg + 1) / 8)
    if rawInput then
        map = code:sub(5, 4 + mapSize)
    else
        local success, mapLen = pcall(string.unpack, '>I4', code:sub(5, 8))
        if not success then
            error('Invalid code', 2)
        end
        success, map = pcall(RLEDecompress, code:sub(9, 8 + mapLen))
        if not success then
            error('Invalid code', 2)
        end
    end
    local L,R,T,B = arm,-arm,leg,-leg
    for c=1, mapSize do
        local byte = map:byte(c)
        for k=0,7 do
            if byte&1>0 then
                local x = ((c - 1) * 8 + k) % (2 * arm + 1) - arm
                local y = math.floor(((c - 1) * 8 + k) / (2 * arm + 1)) - leg
                if x<L then L=x end
                if x>R then R=x end
                if y<T then T=y end
                if y>B then B=y end
            end
            byte = byte >> 1
        end
    end
    return Vector(L+R,T+B)/2
end

---@param code string 编码表示的图像数据
---@param rawInput boolean|nil 表示输入的编码是否为未压缩格式，默认false
---@param rawOutput boolean|nil 表示输出的编码是否为未压缩格式，默认false
---@return string --code: 返回以图像中心为原点重新定位后的图像数据编码
---@nodiscard
function SCMod:CenterizeCode(code,rawInput,rawOutput)
    local success, arm, leg = pcall(string.unpack, '>HH', code:sub(1, 4))
    if not success then
        error('Invalid code', 2)
    end
    local mapSize = math.ceil((2 * arm + 1) * (2 * leg + 1) / 8)
    if not rawInput then
        local success, decompressed = pcall(Decompress, code)
        if not success then
            error(decompressed, 2)
        end
        code = decompressed
    end
    local map = code:sub(5, 4 + mapSize)
    local dots = code:sub(5 + mapSize)

    local L,R,T,B = arm,-arm,leg,-leg
    local MapGrid = {}
    for c=1, mapSize do
        local byte = map:byte(c)
        for k=0,7 do
            if byte&1>0 then
                local x = ((c - 1) * 8 + k) % (2 * arm + 1) - arm
                local y = math.floor(((c - 1) * 8 + k) / (2 * arm + 1)) - leg
                MapGrid[x] = MapGrid[x] or {}
                MapGrid[x][y] = true
                if x<L then L=x end
                if x>R then R=x end
                if y<T then T=y end
                if y>B then B=y end
            end
            byte = byte >> 1
        end
    end
    if L > R then
        L, R = 0, 0
    end
    if T > B then
        T, B = 0, 0
    end
    local center = Vector((L+R)//2,(T+B)//2)

    local new_arm = math.ceil((R-L)/2)
    local new_leg = math.ceil((B-T)/2)
    local newHeader = string.pack('>HH', new_arm, new_leg)

    local newMap = ''
    local byte, cycle = 0, 0
    for y=center.Y-new_leg,center.Y+new_leg do
        for x=center.X-new_arm,center.X+new_arm do
            if MapGrid[x] and MapGrid[x][y] then
                byte = byte | 1 << cycle
            end
            cycle = cycle + 1
            if cycle >= 8 then
                newMap = newMap .. string.char(byte)
                byte, cycle = 0, 0
            end
        end
    end
    if cycle > 0 then
        newMap = newMap .. string.char(byte)
    end
    if rawOutput then
        return newHeader .. newMap .. dots
    end
    return Compress(newHeader, newMap, dots)
end

---@param code string 编码表示的图像数据
---@return fun(): (x:number, y:number, r:number, g:number, b:number, a:number) --iterator: 返回一个迭代器函数，每次调用返回下一个像素的坐标和RGBA值
function SCMod:CodeIter(code,rawInput)
    if not rawInput then
        local success, decompressed = pcall(Decompress, code)
        if not success then
            error(decompressed, 2)
        end
        code = decompressed
    end
    local header = code:sub(1, 4)
    local success, arm, leg = pcall(string.unpack, '>HH', header)
    if not success then
        error('Invalid code', 2)
    end
    local mapSize = math.ceil((2 * arm + 1) * (2 * leg + 1) / 8)
    local map = code:sub(5, 4 + mapSize)
    code = code:sub(5 + mapSize)
    local c,i = 1,0
    return function()
        while c <= mapSize do
            local byte = map:byte(c)
            while i < 8 do
                local x = ((c - 1) * 8 + i) % (2 * arm + 1) - arm
                local y = math.floor(((c - 1) * 8 + i) / (2 * arm + 1)) - leg
                local r,g,b,a = 0,0,0,0
                if byte&(1<<i)>0 then
                    local coDot = code:sub(1, 4)
                    code = code:sub(5)
                    r, g, b, a = DecodeDot(coDot)
                end
                i = i + 1
                return x,y,r,g,b,a
            end
            c = c + 1
            i = 0
        end
    end
end

---@param sprite Sprite
---@param animation string|nil 要编码的动画名称，默认使用精灵的默认动画
---@param arm number|nil 图像中心到水平边缘的最大距离，默认与EncodeSpriteLayer相同
---@param leg number|nil 图像中心到垂直边缘的最大距离，默认与EncodeSpriteLayer相同
---@param maxframe number|nil 要编码的最大帧数，默认编码到动画的最后一帧
---@param specificLayer number|nil 要编码的特定图层索引，默认编码所有图层
---@param rawOutput boolean|nil 是否输出未压缩的编码表示，默认false
---@return table --codes: 返回一个表，包含每一帧的编码表示
---@nodiscard
function SCMod:EncodeSpriteAnimation(sprite,animation,arm,leg,maxframe,specificLayer,rawOutput)
    local success, defaultAnim = pcall(sprite.GetDefaultAnimation, sprite)
    if not success then
        error('Invalid sprite', 2)
    end
    local original_animation = sprite:GetAnimation()
    local original_frame = sprite:GetFrame()
    animation = type(animation) == "string" and animation or defaultAnim
    if type(maxframe)~='number' or maxframe<0 then
        sprite:SetFrame(animation, 0)
        sprite:SetLastFrame()
        maxframe = sprite:GetFrame()
    else
        maxframe = math.ceil(maxframe)
    end
    if specificLayer and (type(specificLayer)~='number' or specificLayer<0 or specificLayer>=sprite:GetLayerCount()) then
        error('Invalid layer', 2)
    end
    local encoder = specificLayer and function()return self:EncodeSpriteLayer(sprite,specificLayer,arm,leg,rawOutput)end or function()return self:EncodeSprite(sprite,arm,leg,rawOutput)end
    local codes = {}
    for frame = 0, maxframe do
        sprite:SetFrame(animation, frame)
        success, codes[frame] = pcall(encoder)
        if not success then
            sprite:Play(original_animation, true)
            sprite:SetFrame(original_frame)
            error('Frame '..frame..' error: '..codes[frame], 1)
        end
    end
    sprite:Play(original_animation, true)
    sprite:SetFrame(original_frame)
    return codes
end

---@param codes table 包含每一帧编码表示的表
---@param endless boolean|nil 是否无限循环迭代，默认false
---@param rawInput boolean|nil 表示输入的编码是否为未压缩格式，默认false
---@param rawOutput boolean|nil 表示输出的编码是否为未压缩格式，默认false
---@return fun(): (frame:number, code:string) --iterator: 返回一个迭代器函数，每次调用返回当前帧数和对应的编码表示
---@nodiscard
function SCMod:FrameIter(codes,endless,rawInput,rawOutput)
    if type(codes)~='table' then
        error('Invalid codes', 2)
    end
    local maxframe = #codes
    local currentFrame = -1
    return function()
        currentFrame = currentFrame + 1
        if currentFrame > maxframe and endless then
            currentFrame = 0
        end
        local success, code = pcall(self.ReshapeCode,self,codes[currentFrame],nil,rawInput,rawOutput)
        if not success or not code then
            error('Error at frame '..currentFrame, 2)
        end
        return currentFrame,code
    end
end

local Mod4SavingData = RegisterMod('SpriteCodec_DataSaver', 1)
---@param code string 编码表示的图像数据
---@param rawInput boolean|nil 表示输入的编码是否为未压缩格式，默认false
function SCMod:SaveAsPNG(code, rawInput)
    if type(code) ~= 'string' then
        error('Code isn\'t a string', 2)
    end
    if rawInput then
        local header, map, dots = code:sub(1,4)
        local success, arm, leg = pcall(string.unpack, '>HH', header)
        if not success then
            error('Invalid code', 2)
        end
        local mapSize = math.ceil((2 * arm + 1) * (2 * leg + 1) / 8)
        map = code:sub(5, 4 + mapSize)
        dots = code:sub(5 + mapSize)
        code = Compress(header, map, dots)
    end
    code = self:Share(code)
    Mod4SavingData:SaveData('png:'..code)
end

---@param codes table 包含每一帧编码表示的表
---@param animationName string|nil 动画名称，默认使用'spritecodec_saved_animation'
---@param rawInput boolean|nil 表示输入的编码是否为未压缩格式，默认false
function SCMod:SaveAsAnm2(codes, animationName, rawInput)
    if type(codes) ~= 'table' then
        error('Codes must be a table', 2)
    end
    animationName = animationName or 'spritecodec_saved_animation'

    local processed = {}

    for i, code in ipairs(codes) do
        if type(code) ~= 'string' then
            error('Frame '..i..' code isn\'t a string', 2)
        end
        if rawInput then
            local header = code:sub(1,4)
            local success, arm, leg = pcall(string.unpack, '>HH', header)
            if not success then
                error('Invalid code at frame '..i, 2)
            end
            local mapSize = math.ceil((2 * arm + 1) * (2 * leg + 1) / 8)
            local map = code:sub(5, 4 + mapSize)
            local dots = code:sub(5 + mapSize)
            code = Compress(header, map, dots)
        end
        code = self:Share(code)
        table.insert(processed, code)
    end

    local finalData = 'anm2:'..animationName..':'..table.concat(processed, '|')
    Mod4SavingData:SaveData(finalData)
end
----------------------------------------------------------------------------------------
SpriteCodec = {}
setmetatable(SpriteCodec, {
    __index = function(t,k)return SCMod[k]end,
    __newindex = function(t, k, v)
        error("Attempt to modify read-only table", 2)
    end,
    __tostring = function()return 'SpriteCodec v'..SCMod.Version..' - Keye3Tuido\n' end,
    __pairs = function(t)
        return SCMod:MemberIter()
    end,
    __ipairs = function(t)
        return SCMod:MemberIter()
    end,
    __metatable = "This metatable is locked"
})
Isaac.ConsoleOutput(tostring(SpriteCodec))