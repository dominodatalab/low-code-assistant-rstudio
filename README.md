# lca

```
t1 <- FilterTransformation$new("gear", ">", 4, "dfcar")
t2 <- SelectTransformation$new(c("wt", "gear", "hp"), "postselect")
t3 <- DropTransformation$new(c("disp", "hp"), "postdrop")

dfcar <- mtcars
res1 <- TransformationSequence$new(list(t1, t2, t3), "dfcar")$run()

dfcar <- mtcars
res2 <- TransformationSequence$new(list(t1, t3, t2), "dfcar")$run()

```
