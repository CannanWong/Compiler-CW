file="src/test/scala/wacc/front_end/valid/IO/print/multipleStringsAssignment.wacc"
extracted=$(sed -n '/# Output:/,/# Program:/ p' "$file")
expected=$(echo "$extracted" | head -n -3 | tail -n +2 | cut -c 3-)
echo "$expected"

file="src/test/scala/wacc/back_end/exitBasic.wacc"
extracted=$(sed -n '/# Output:/,/# Exit:/ p' "$file")
expected=$(echo "$extracted" | head -n -3 | tail -n +2 | cut -c 3-)
echo "$extracted"