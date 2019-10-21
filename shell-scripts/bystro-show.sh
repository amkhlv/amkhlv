#!/bin/bash

read -r -d '' JQ <<'EOF'
if .arglist then 
   if .multipage then [(.name+"/index.html"), .arglist] 
   else [(.destination+"/"+.name), .arglist] 
   end 
else 
   if .multipage then (.name+" →→→ "+.name+"/index.html") 
   else (.name+" → "+.destination+"/"+.name+".html") 
   end 
end
EOF

bystrotex -s | jq -r "$JQ"
