#!/usr/bin/env ruby
# Works with IFTTT recipe https://ifttt.com/recipes/125999
#
# Set Hazel to watch the folder you specify in the recipe.
# Make sure nvALT is set to store its notes as individual files.
# Edit the $target_folder variable below to point to your nvALT
# ntoes folder.
require 'date'
require 'open-uri'
require 'net/http'
require 'fileutils'
require 'cgi'
 
$target_folder = "~/Dropbox/Notational Data/"
$source_folder = "~/Dropbox/IFTTT/nvALT/"
 
def url_to_markdown(url)
  res = Net::HTTP.post_form(URI.parse("http://heckyesmarkdown.com/go/"),{'u'=>url,'read'=>'1'})
  if res.code.to_i == 200
    res.body
  else
    false
  end
end
 
#file = ARGV[0]
 
begin
  Dir.glob(File.expand_path($source_folder) + '/*.txt') do |file|
    input = IO.read(file).force_encoding('utf-8')
   
    headers = {}
    input.each_line {|line|
      key, value = line.split(/: /)
      headers[key] = value.strip || ""
    }
   
    outfile = File.join(File.expand_path($target_folder), headers['Title'].gsub(/["!*?'|]/,'') + ".md")
   
    date = Time.now.strftime("%Y-%m-%d %H:%M")
    date_added = Date.parse(headers['Date']).strftime("%Y-%m-%d %H:%M")
    content = "Title: #{headers['Title']}\nDate: #{date}\nDate Added: #{date_added}\nSource: #{headers['URL']}\n"
   
    tags = false
    if headers['Tags'].length > 0
      tag_arr = headers['Tags'].split(", ")
      tag_arr.map! {|tag|
        %Q{"#{tag.strip}"}
      }
      tags = tag_arr.join(" ")
      content += "Keywords: #{tags}\n"
    end
   
    markdown = url_to_markdown(headers['URL']).force_encoding('utf-8')
   
    if markdown
      content += headers['Image'].length > 0 ? "\n\n![](#{headers['Image']})\n\n#{markdown}\n" : "\n\n"+markdown
    else
      content += headers['Image'].length > 0 ? "\n\n![](#{headers['Image']})\n\n#{headers['Excerpt']}\n" : "\n\n"+headers['Excerpt']
    end
   
    File.open(outfile,'w') {|f|
      f.puts content
    }
   
    if tags && File.exists?("/usr/local/bin/openmeta")
      %x{/usr/local/bin/openmeta -a #{tags} -p "#{outfile}"}
    end
    FileUtils.rm(file)
  end
rescue Exception => e
  puts e
end
