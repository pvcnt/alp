all:
	./sbt "project privamov-runtime" assembly
	mkdir -p dist && mv modules/privamov-runtime/target/scala-2.11/privamov-runtime-bin.jar dist/alp.jar

clean:
	rm -f dist/alp.jar