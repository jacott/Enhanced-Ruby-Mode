#!/usr/bin/ruby

class BufferStore
  def initialize
    @buffers={}
  end

  def get_buffer(buf_num)
    @buffers[buf_num] ||
     @buffers[buf_num]=ErmBuffer.new
  end

  def rm(buf_num)
    @buffers.delete(buf_num)
  end
end

# module Kernel
#   def fixme(*args)
#     $fixme.puts args.inspect
#     $fixme.flush
#   end
# end

STDIN.set_encoding("UTF-8")

# $fixme=File.open("/tmp/erm.out",'w')
# $fixme.puts "\n\nstarting\n\n"
# $fixme.flush

require_relative 'erm_buffer'

store=BufferStore.new
begin
  while c=STDIN.gets("\n\0\0\0\n")
    # fixme c
    cmd=c[0].to_sym
    args=c[1..-6].split(':',6)
    buf=store.get_buffer(bn=args.shift.to_i)
    case cmd
    when :c
      STDOUT.puts 'c'
      STDOUT.flush # need to flush early because ruby writes warnings directly to STDOUT
      STDOUT.puts "#{buf.check_syntax}\n\n\0\0\0"
      STDOUT.flush
    when :k
      store.rm(bn)
      # $fixme.puts "Killed"
    else
      buf.add_content(cmd,*args) unless cmd == :g
      unless cmd == :a
        r=buf.parse
        # fixme r
        STDOUT.puts(r << "\n\0\0\0")
        STDOUT.flush
      end
    end
    # $fixme.puts buf.buffer
    # $fixme.flush
  end
rescue
  # $fixme.puts c.inspect
  # $fixme.puts $!.message
  # $fixme.puts $!.backtrace
  # $fixme.flush
  STDOUT.puts "e#{$!.message}: #{$!.backtrace.join("\n")}\n\0\0\0\n"
  STDOUT.flush
end

