Given(/^the program has finished$/) do
  @cucumber = `clisp example.lisp`
end

Then(/^the output is correct for each test$/) do
  lines = @cucumber.split("\n")

  lines.length.should == 17

  banner = 'Starting tests with seed
#S(RANDOM-STATE'

  lines[0 .. 1].join("\n").should == banner

  seed = /   #\*[01]{64}\)/

  lines[2].should =~ seed

  lines[3].should =~ /^[\.@X\-]+$/

  lines[4].should == 'ERROR (ISNT MONEY= (DOLLARS \'M) (FRANCS M))'
  lines[5 .. 7].join("\n").should == '
=: M is not a number
'
  lines [8].should =~ /^  with values \#S\(MONEY \:AMOUNT M \:CURRENCY USD\) \#S\(MONEY \:AMOUNT (-)?[0-9]+ \:CURRENCY FRANC\)$/
  lines[9].should =~ /^  for \(\(M (-)?[0-9]+\)\)$/
  lines[10 .. 11].join("\n").should == '  100/100 counterexamples.
FAIL (IS MONEY= (DOLLARS M) (DOLLARS N))'
  lines[12].should =~ /^  with values \#S\(MONEY \:AMOUNT (-)?[0-9]+ \:CURRENCY USD\) \#S\(MONEY \:AMOUNT (-)?[0-9]+ \:CURRENCY USD\)$/
  lines[13].should =~ /^  for \(\(M (-)?[0-9]+\) \(N (-)?[0-9]+\)\)$/

  lines[14].should =~ /^  [0-9]+\/100 counterexamples\.$/
  lines[15].should == '  0 cases checked and passed in 100 attempts.'
  lines[16].should == '95 tests submitted; 2 FAILED.'
end
