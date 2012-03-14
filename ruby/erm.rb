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

module Kernel
  def fixme(*args)
    $fixme.puts args.inspect
  end
end

STDIN.set_encoding("binary")

File.open("/tmp/erm.out",'a') do |out|
  $fixme=out
  $fixme.puts "\n\nstarting\n\n"
  $fixme.flush
  require_relative 'erm_buffer'

  store=BufferStore.new
  begin
    while c=STDIN.gets("\n\0\0\0\n")
      # $fixme.puts
      cmd=c[0].to_sym
      args=c[1..-6].split(':',6)
      buf=store.get_buffer(bn=args.shift.to_i)
      if cmd == :k
        store.rm(bn)
        # $fixme.puts "Killed"
      else
        buf.add_content(cmd,*args) unless cmd == :g
        unless cmd == :a
          r=buf.parse
          # fixme r
          puts(r << "\n\0\0\0\n")
        end
      end
      # $fixme.puts buf.buffer
      # $fixme.flush
    end
  rescue
    $fixme.puts $!.message
    $fixme.puts $!.backtrace
    $fixme.flush
    puts "#{$!.message}: #{$!.backtrace.join("\n")}".inspect << "\n\0\0\0\n"
  end
end
