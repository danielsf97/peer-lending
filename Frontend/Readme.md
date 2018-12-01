#Fazer build do gpb
-> gpb/make

#Compilar protos
-> gpb/bin/protoc-erl -I. protos.proto
-> info: https://github.com/tomas-abrahamsson/gpb/blob/master/README.md?fbclid=IwAR2-R9sjoR7VZYR3FHfH5OeR_oeCiDVIyoYQ856GTamuDTxXSe-HzZ7jkWU
-> erlc -I gpb/include protos.erl

#Build rebar
-> git clone git://github.com/rebar/rebar.git
-> cd rebar
-> ./bootstrap
-> sudo cp rebar /usr/bin
-> Remover git rebar

#Build zeromq
-> git clone http://github.com/zeromq/erlzmq2.git
-> cd erlzmq2
-> make