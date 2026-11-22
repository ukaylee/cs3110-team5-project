Quitting with "quit" prints the trip summary, the help hint, and says goodbye.

  $ printf 'Trip A\nquit\n' | ../bin/main.exe
  Trip "Trip A": 0 traveler(s), 0 expense(s).
  Type "help" for commands or "quit" to leave.
  Goodbye!

Pressing Enter with no name falls back to "Untitled trip".

  $ printf '\nquit\n' | ../bin/main.exe
  Trip "Untitled trip": 0 traveler(s), 0 expense(s).
  Type "help" for commands or "quit" to leave.
  Goodbye!

Blank lines between commands are ignored and produce no output.

  $ printf 'Trip C\n\nquit\n' | ../bin/main.exe
  Trip "Trip C": 0 traveler(s), 0 expense(s).
  Type "help" for commands or "quit" to leave.
  Goodbye!

"help" prints the full command list.

  $ printf 'Trip D\nhelp\nquit\n' | ../bin/main.exe
  Trip "Trip D": 0 traveler(s), 0 expense(s).
  Type "help" for commands or "quit" to leave.
  Available commands:
    add-traveler NAME
    remove-traveler NAME
    add-expense PAYER CATEGORY AMOUNT SPLIT | DESCRIPTION
    add-bill PAYER CATEGORY tax=X tip=Y | DESCRIPTION | ITEM PRICE PEOPLE
    edit-expense ID PAYER CATEGORY AMOUNT SPLIT | DESCRIPTION
    delete-expense ID
    expenses
    balances
    settle
    budget CATEGORY AMOUNT
    report
    help
    quit
  Goodbye!

An unrecognized command reports a parse error but keeps the session alive.

  $ printf 'Trip E\nnonsense\nquit\n' | ../bin/main.exe
  Trip "Trip E": 0 traveler(s), 0 expense(s).
  Type "help" for commands or "quit" to leave.
  Error: Unrecognized command. Type 'help' for available commands.
  Goodbye!

A Session-level error (e.g. deleting an expense that doesn't exist) is reported
the same way and the session still continues.

  $ printf 'Trip F\ndelete-expense 99\nquit\n' | ../bin/main.exe
  Trip "Trip F": 0 traveler(s), 0 expense(s).
  Type "help" for commands or "quit" to leave.
  Error: there is no expense with id 99
  Goodbye!

End-to-end flow: add travelers, log an equal-split expense, check balances,
then settle up.

  $ printf 'Weekend\nadd-traveler Amy\nadd-traveler Sandy\nadd-expense Amy dining 10.00 equal all | Lunch\nbalances\nsettle\nquit\n' | ../bin/main.exe
  Trip "Weekend": 0 traveler(s), 0 expense(s).
  Type "help" for commands or "quit" to leave.
  Added traveler: Amy
  Added traveler: Sandy
  Added expense: Lunch
  Amy is owed $5.00
  Sandy owes $5.00
  Sandy pays Amy $5.00
  Goodbye!

Wrong number of command-line arguments exits with a usage message and status 1.

  $ ../bin/main.exe --load
  Usage: main.exe [--load FILE]
  [1]

  $ ../bin/main.exe foo bar
  Usage: main.exe [--load FILE]
  [1]

Loading a nonexistent file exits with status 1 and reports the OS error.

  $ ../bin/main.exe --load /nonexistent/triptab-cram-fixture.trip
  Could not load /nonexistent/triptab-cram-fixture.trip: /nonexistent/triptab-cram-fixture.trip: No such file or directory
  [1]
