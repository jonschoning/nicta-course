while inotifywait -m -q -e modify src/Course/JsonParser.hs; do
  ./test.sh
done

