echo -e "Tests:\n"
allpass=1
for file in src/test/scala/wacc/*.wacc
do 
    echo "Testing $file:"
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

    output=$(./compile $file)
    exit=$?
    echo "Expected output: $expected_output"
    echo "Output: $output"
    echo "Expected exit: $expected_exit"
    echo "Exit: $exit"
    
    pass=1

    # Check if program outputs correct error
    if echo $output | grep -q $expected_output
        then echo "Output correct"
    else
        echo "Output wrong!"
        pass=0
    fi

    # Check if program outputs correct exit code
    if [ $exit -eq $expected_exit ]
        then echo "Exit code correct"
    else
        echo "Exit code wrong!"
        pass=0
    fi

    if [ $pass -eq 1 ]
        then echo -e "Test passed\n"
    else
        echo -e "Test failed!\n"
        allpass=0
    fi
done

if [ $allpass -eq 1 ]
    then exit 0
else
    exit 1
fi