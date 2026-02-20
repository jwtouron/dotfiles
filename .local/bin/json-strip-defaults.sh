#!/bin/sh

jq --slurp '
  def prune($cur; $def):
    if ($cur|type) != ($def|type) then $cur
    elif ($cur|type) == "object" then
      (reduce ($cur|keys_unsorted[]) as $k ({}; 
        if ($cur[$k] == $def[$k]) then .
        else . + {($k): prune($cur[$k]; $def[$k])}
        end
      ) | if length==0 then empty else . end)
    elif ($cur|type) == "array" then
      if $cur == $def then empty else $cur end
    else
      if $cur == $def then empty else $cur end
    end;
  prune(.[1]; .[0])
' "$1" "$2"
