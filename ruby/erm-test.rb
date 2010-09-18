prog = IO.read("/home/geoffj/tmp/a.rb")
c = "p0:#{prog.size}:"
c << "\n\0\0\0\n"
File.open("/home/geoffj/tmp/a.rb.out",'w') {|io|
  io.write(c)
}
