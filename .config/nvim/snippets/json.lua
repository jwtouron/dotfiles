local function desc(s)
  return "[JSON Schema] " .. s
end

return {
  {
    prefix = "props",
    body = {
      '"properties": {',
      '\t"${1:name}": {',
      '\t\t"type": "${2|array,boolean,integer,null,number,object,string|}"',
      '\t}$0',
      '}',
    },
    desc = desc("Properties"),
  },

  {
    prefix = "propspat",
    body = {
      '"properties": {',
      '\t"${1:name}": {',
      '\t\t"type": "string",',
      '\t\t"pattern": "${2}"',
      '\t}$0',
      '}',
    },
    desc = desc("Properties with Pattern")
  },

  {
    prefix = "prop",
    body = {
      '"${1:name}": {',
      '\t"type": "${2|array,boolean,integer,null,number,object,string|}"',
      '}$0',
    },
    desc = desc("Property")
  },

  {
    prefix = "proppat",
    body = {
      '"${1:name}": {',
      '\t"type": "string",',
      '\t"pattern": "${2}"',
      '}$0',
    },
    desc = desc("Property with Pattern")
  },

  {
    prefix = "items",
    body = {
      '"items": {',
      '\t"type": "${2|array,boolean,integer,null,number,object,string|}"$0',
      '}',
    },
    desc = desc("items")
  },

  {
    prefix = "required",
    body = {
      '"required": [',
      '\t"${1:item}"$0',
      ']',
    },
    desc = desc("required")
  },

  {
    prefix = "add-props-false",
    body = '"additionalProperties": false$0',
    desc = desc("additionalProperties = false")
  },
}
