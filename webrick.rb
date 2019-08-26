
require 'webrick'

srv = WEBrick::HTTPServer.new({
  DocumentRoot: './',
  BindAddress: "192.168.11.140",
  Port:        8001,
})

srv.start

