module Canapi.MimeTypeList where

import qualified Network.HTTP.Media as HttpMedia

binary :: [HttpMedia.MediaType]
binary = ["application/octet-stream"]

html :: [HttpMedia.MediaType]
html = ["text/html", "application/xhtml+xml"]

json :: [HttpMedia.MediaType]
json = ["application/json", "text/json"]

text :: [HttpMedia.MediaType]
text = ["text/plain"]

xhtml :: [HttpMedia.MediaType]
xhtml = ["application/xhtml+xml", "text/html"]

xml :: [HttpMedia.MediaType]
xml = ["application/xml", "text/xml"]

yaml :: [HttpMedia.MediaType]
yaml = ["application/yaml", "text/vnd.yaml", "text/yaml", "text/x-yaml", "application/x-yaml"]
