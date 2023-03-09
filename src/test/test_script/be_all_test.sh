echo -e "Tests:\n"
allpass=1
passcount=0
failcount=0
validcount=221

shopt -s globstar
for file in src/test/scala/wacc/back_end_all/valid/**/*.wacc
do
    expected_exit=0
    expected_output=""
    exit_line=$(grep -A1 "# Exit" $file)
    if [ -n "$exit_line" ]
        then
        expected_exit=$(echo $exit_line | grep -o -E '[0-9]+')
        extracted=$(sed -n '/# Output:/,/# Exit:/ p' "$file")
        expected_output=$(echo "$extracted" | head -n -2 | tail -n +2 | cut -c 3-)
    else
        extracted=$(sed -n '/# Output:/,/# Program:/ p' "$file")
        expected_output=$(echo "$extracted" | head -n -2 | tail -n +2 | cut -c 3-)        
    fi

    timeout 10s ./compile $file > /dev/null
    filename=$(basename $file .wacc)
    timeout 10s arm-linux-gnueabi-gcc -o $filename -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $filename.s
    output=$(timeout 10s qemu-arm -L /usr/arm-linux-gnueabi/ $filename)
    exit=$?
    
    pass=1

    # Check program output
    if [ "$output" == "$expected_output" ]
        then : 
    else
        pass=0
    fi

    # Check exit code
    if [ "$exit" == "$expected_exit" ]
        then : 
    else
        pass=0
    fi

    if [ $pass -eq 1 ]
        then 
        echo -e "Test $file passed"
        ((passcount=passcount+1))
    else
        echo -e "Test $file failed!"
        ((failcount=failcount+1))
        allpass=0
    fi
done

echo -e -n "Current progress: "
echo "scale=2;100*$passcount/$validcount" | bc | tr '\n' ' '
echo -e "% of tests in wacc_example/valid passed"

if [ $allpass -eq 1 ]
    then 
    echo -e "All $passcount tests passed"
    exit 0
else
    echo -e "$failcount tests failed! $passcount tests passed"
    exit 1
fi