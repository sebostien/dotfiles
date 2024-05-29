function ddg --description 'Search DuckDuckGo'
  set url "https://duckduckgo.com/?q="

  for arg in $argv
    set url "$url$arg+"
  end

  xdg-open "$url"
end
