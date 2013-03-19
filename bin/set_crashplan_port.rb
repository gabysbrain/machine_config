#!/usr/bin/ruby

# set the crashplan port to the desired one.
# USAGE: set_crashplan_port.rb <port number>

CP_FILE = "/Users/tom/Library/Application Support/CrashPlan/ui.properties"

# from http://dzone.com/snippets/load-properties-properties
def load_properties(properties_filename)
  properties = {}
  File.open(properties_filename, 'r') do |properties_file|
    properties_file.read.each_line do |line|
      line.strip!
      if (line[0] != ?# and line[0] != ?=)
        i = line.index('=')
        if (i)
          properties[line[0..i - 1].strip] = line[i + 1..-1].strip
        else
          properties[line] = ''
        end
      end
    end      
  end
  properties
end

def write_properties(fname, props)
  File.open(fname, 'w') do |props_file|
    props.each_pair do |k,v|
      props_file.write("#{k}=#{v}\n")
    end
  end
end

props = load_properties(CP_FILE)
props['servicePort'] = ARGV[0]
write_properties(CP_FILE, props)

