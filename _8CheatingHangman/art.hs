module Art where

fail1 = "\n\n\n\n\n  ____________\n\n"
fail2 = "\n |\n |\n |\n |\n |\n |\n |\n |\n |\n |____________"
fail3 = "  _____\n |\n |\n |\n |\n |\n |\n |\n |\n |\n |____________"
fail4 = "\n  _____\n |     |\n |\n |\n |\n |\n |\n |\n |\n |\n |____________"
fail5 = "\n  _____\n |    _|_\n |   /   \\\n |   \\___/\n |   \n |\n |\n |\n |\n |\n |____________"
fail6 = "\n  _____\n |    _|_\n |   /   \\\n |   \\___/\n |     |\n |     |\n |     |\n |\n |\n |\n |____________"
fail7 = "\n  _____\n |    _|_\n |   /   \\\n |   \\___/\n |     | _/\n |     |/\n |     |\n |\n |\n |\n |____________"
fail8 = "\n  _____\n |    _|_\n |   /   \\\n |   \\___/\n |  \\_ | _/\n |    \\|/\n |     |\n |\n |\n |\n |____________"
fail9 = "\n  _____\n |    _|_\n |   /   \\\n |   \\___/\n |  \\_ | _/\n |    \\|/\n |     |\n |    /\n |   /\n |\n |____________"
fail10 = "\n  _____\n |    _|_\n |   /   \\\n |   \\___/\n |  \\_ | _/\n |    \\|/\n |     |\n |    / \\\n |   /   \\\n |\n |____________"

getFail :: Int -> String
getFail 0 = fail10
getFail 1 = fail9
getFail 2 = fail8
getFail 3 = fail7
getFail 4 = fail6
getFail 5 = fail5
getFail 6 = fail4
getFail 7 = fail3
getFail 8 = fail2
getFail 9 = fail1
getFail _ = ""