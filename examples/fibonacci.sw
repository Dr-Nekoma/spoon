[defun rec fibonacci [(n Integer)]
    [match n
      [-> 0 0]
      [-> 1 1]
      [-> n [+ [fibonacci [- n 1]] [fibonacci [- n 2]]]]]]

#[1 2 ![+ 1 2]]
#[1 2 3]
[list 1 2 3]
