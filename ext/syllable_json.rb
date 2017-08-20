# encoding: utf-8
require 'json'

out = {}

File.readlines('cmudict.txt', encoding: 'iso-8859-1').each do |line|
  case line
  when /^[^A-Z]/
    next
  when /[A-Z]+\(\d\)/
    next
  else
    toks = line.split(/\s+/)
    term = toks.shift
    syllables = toks.select {|t| t =~ /\d$/}.count
    out[term.downcase] = syllables
  end
end

File.open('../terms.json', 'w') do |file|
  file.write(JSON.generate(out))
end
