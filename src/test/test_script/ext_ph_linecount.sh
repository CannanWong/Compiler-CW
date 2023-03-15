# Compare line count between original and peephole optimised version

echo -e "Line count:\n"
file_count=0
line_count=0
optimised_line_count=0
total_line_count=0
optimised_total_line_count=0

shopt -s globstar
for file in src/test/scala/wacc/back_end/**/*.wacc
do
    filename=$(basename $file .wacc)

    # Original
    timeout 10s ./compile $file > /dev/null
    line_count=$(wc -l < $filename.s)
    ((total_line_count=total_line_count+line_count))

    # Optimised
    timeout 10s ./compile $file -p > /dev/null
    optimised_line_count=$(wc -l < $filename.s)
    echo -e "Line count optimised from $line_count to $optimised_line_count in $filename.s"
    ((optimised_total_line_count=optimised_total_line_count+optimised_line_count))
    ((file_count=file_count+1))
done

echo -e "Statistics: "
echo -e "Total line count is optimised from $total_line_count to $optimised_total_line_count in $file_count files"
((reduced_line_count=total_line_count-optimised_total_line_count))
echo -e "A total of $reduced_line_count lines is reduced"
echo -e -n "Average line count is optimised from "
echo "scale=2;$total_line_count/$file_count" | bc | tr '\n' ' '
echo -e -n "to "
echo "scale=2;$optimised_total_line_count/$file_count" | bc | tr '\n' ' '
echo " "
echo "scale=2;100*$reduced_line_count/$total_line_count" | bc | tr '\n' ' '
echo -e "% of lines is reduced"
