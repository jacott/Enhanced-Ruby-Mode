require 'ripper'

def invalid_syntax?(code, fname)
  code.force_encoding("utf8")
  code = code.sub(/\A(?:\s*\#.*$)*(\n)?/n) {
    "#$&#{"\n" if $1 && !$2}BEGIN{return false}\n"
  }
  eval(code, nil, fname, 0)
rescue Exception
  $!.message
end

