
# Indentation

## C-M-q indents

```r case
  {
          fun¶_call(
argument
) +
stuff1
 } +
stuff2
```

```elisp
"C-M-q"
```

```r result
  {
          fun¶_call(
              argument
          ) +
              stuff1
 } +
stuff2
```

Prefix argument causes a climb to top-level before indentation.

```elisp
"C-u"
"C-M-q"
```

```r result
{
    fun¶_call(
        argument
    ) +
        stuff1
} +
    stuff2
```


## Other test...

```r case
foo + bar¶
```

```elisp
(ignore 1)
```


Local Variables:
mode2: ess-r
End:
