function Image (img)
  img.caption[1] = pandoc.Strong(img.caption[1])
  img.caption[3] = pandoc.Strong(pandoc.Str(string.gsub(img.caption[3].text, ":", ".")))
  return img
end
