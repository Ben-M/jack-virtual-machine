PHONY: all
all:
	@echo Running  Makefile
	@chmod 755 VMTranslator

build:
	@sbt assembly
	@mv ./target/scala-2.12/jackvm-assembly-1.0.jar ./VMTranslator.jar
	@zip upload.zip VMTranslator.jar VMTranslator Makefile lang.txt

