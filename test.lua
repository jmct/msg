print('Hello, from a Lua file! Manuel and Gus')

print('And another thing!')

-- perhaps pre-create `conversion` and `site` tables before loading the configuration file
conversion = {}
conversion["md"]  = {"html"}
conversion["lhs"] = {"html", "lhs"}

posts = {}
posts["source"] = "posts"
posts["conversion"] = conversion

site = {}
site["root"] = "my_site"
site["dirs"] = { posts }

test_table = {5,3,1}
test_table["fun"] = {'a','b','c'}
test_table["site"] = site

test_val = 42

-- TODO: Figure out how to detect that the io functions won't work!
-- Currently it just fails silently!
-- print('TODO: investigate')
-- f = io.open("text.txt", "r")
-- print('After open')
-- print(f:read(f))
-- print('After read')
