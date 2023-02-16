echo -e "Tests:\n"
allpass=1
passcount=0
failcount=0
validcount=221
invalidcount=133

shopt -s globstar
for file in src/test/scala/wacc/back_end/**/*.wacc
do 
    expected=$(grep -A1 "# Output" $file)
    if echo $expected | grep -q "# #syntax_error"
        then
        expected_output="syntax_error"
        expected_exit=100
    elif echo $expected | grep -q "# #semantic_error"
        then
        expected_output="semantic_error"
        expected_exit=200
    else
        expected_output="WACC_45"
        expected_exit=0
    fi

    ./compile $file
    filename=$(basename $file .wacc)
    arm-linux-gnueabi-gcc -o $filename -mcpu=arm1176jzf-s -mtune=arm1176jzf-s src/test/scala/wacc/back_end/$filename.s
    output=$(qemu-arm -L /usr/arm-linux-gnueabi/ $filename)
    exit=$?
    
    pass=1

    # # Check program output
    # if echo $output | grep -q $expected_output
    #     then : # echo "Output correct"
    # else
    #     # echo "Output wrong!"
    #     pass=0
    # fi

    # Check exit code
    if [ $exit -eq $expected_exit ]
        then : # echo "Exit code correct"
    else
        # echo "Exit code wrong!"
        pass=0
    fi

    if [ $pass -eq 1 ]
        then 
        echo -e "Test $file passed"
        ((passcount=passcount+1))
    else
        echo -e "Test $file failed! Exited $exit instead of $expected_exit"
        ((failcount=failcount+1))
        allpass=0
    fi
done

echo -e -n "Current progress: "
echo "scale=2;100*$passcount/($validcount+$invalidcount)" | bc | tr '\n' ' '
echo -e "% of tests in wacc_example passed"

if [ $allpass -eq 1 ]
    then 
    echo -e "All $passcount tests passed"
    exit 0
else
    echo -e "$failcount tests failed! $passcount tests passed"
    exit 1
fi