// A lazyscript program is an IO action.
// The primitive IO actions are
//
// returnIO :: a -> IO a
// Which simply wraps a value in IO.
//
// getLine :: IO String
// Represents the action of reading a line from the world.
//
// putLine :: String -> IO ()
// Represents the action of outputting a line

//    main :: IO a
const main1 = putLine('hi!');

// Same thing written differently
const main2 = returnIO('hi!').flatMap(putLine);

// Same thing yet again.
const main3 = returnIO('HI!')
	.map(x => x.toLowerCase())
	.flatMap(putLine);

//
const main4 = putLine("What's your name?")
	.then(getLine)
	.map(name => 'Welcome, ' + name + '!')
	.flatMap(putLine);
