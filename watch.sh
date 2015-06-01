inotifywait -m -r -q -e modify --format '%w/%f' src/ | while read FILE
do
  doctest -isrc -fno-warn-type-defaults "$FILE"
done

