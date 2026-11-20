# TripTab
A command-line expense-sharing application built in OCaml for CS 3110. TripTab helps groups manage shared trip expenses, split costs among travelers, track budgets, and calculate who owes whom.

## CS3110 Team Project: Team 5 - Slay Team Members
- Kaylee Ulep (klu6)
- Amy Yang (aly32)
- Jennifer Ng (jn576)
- Sandy Yang (syy6)
- Elizabeth Chen (ec975)

## Features

- **Traveler management:** Add and remove travelers from a trip.
- **Expense tracking:** Add, edit, delete, and list expenses.
- **Flexible expense splitting:** Split expenses equally among all travelers, equally among selected travelers, or using exact amounts.
- **Itemized bill splitting:** Assign individual items to specific travelers and distribute tax and tip proportionally.
- **Balance calculations:** View each traveler's net balance based on what they paid and owe.
- **Settlements:** Calculate payments needed to settle debts between travelers.
- **Budget tracking:** Set spending budgets for different expense categories.
- **Expense reports:** View summaries of spending by category and traveler.
- **Data persistence:** Save and load trip data.

## Running the Project

Make sure OCaml and Dune are installed.

Build the project:

```bash
dune build
```

Run the application:

```bash
dune exec bin/main.exe
```

Run the tests:

```bash
dune runtest
```

## Available Commands

| Command | Description |
|---|---|
| `add-traveler NAME` | Add a traveler |
| `remove-traveler NAME` | Remove a traveler |
| `add-expense PAYER CATEGORY AMOUNT SPLIT \| DESCRIPTION` | Add an expense |
| `add-bill PAYER CATEGORY tax=X tip=Y \| DESCRIPTION \| ITEM PRICE PEOPLE` | Add an itemized bill |
| `edit-expense ID PAYER CATEGORY AMOUNT SPLIT \| DESCRIPTION` | Edit an expense |
| `delete-expense ID` | Delete an expense |
| `expenses` | List all expenses |
| `balances` | Display travelers' net balances |
| `settle` | Calculate settlement payments |
| `budget CATEGORY AMOUNT` | Set a category budget |
| `report` | Generate an expense report |
| `help` | Display available commands |
| `quit` | Exit the application |

## Testing

The project uses OUnit for unit testing. Tests cover trip management, expense splitting, itemized bills, command execution, balance calculations, settlements, budgets, and error handling.

Run all tests with:

```bash
dune runtest
```

## Team

Team 5 (Team Slay) — Cornell CS 3110, Fall 2026.
