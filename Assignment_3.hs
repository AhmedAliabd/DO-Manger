printTasks [] = do
  putStr ""
printTasks (x : xs) = do
  putStrLn $ " " ++ x
  printTasks xs

searchTasks [] input = do
  putStrLn "Task not found!"
searchTasks (x : xs) input = do
  if input == x then putStrLn $ " found " ++ x else searchTasks xs input

buildTasksList tasks = do
  putStrLn "Below are the options: \n   add\n   print\n   search\nEnter an option: "
  input <- getLine
  if input == "add"
    then do
      putStrLn "Enter a task"
      task <- getLine
      let newTask = task : tasks
      buildTasksList newTask
    else
      if input == "print"
        then do
          putStrLn "Here are the tasks: "
          printTasks tasks
        else
          if input == "search"
            then do
              putStrLn "Search for a task"
              searchTerm <- getLine
              searchTasks tasks searchTerm
            else do
              putStrLn "Error"
              buildTasksList tasks
  buildTasksList tasks

main = do
  buildTasksList []
