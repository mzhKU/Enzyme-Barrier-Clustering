data(Oxboys, package="nlme")

xyplot(height~age|Subject, data=Oxboys, strip=F, aspect="xy", pch=16, grid=T,
    panel=function(x, y, ...)
    {
        panel.xyplot(x, y, ...)
    #    #panel.lines(x, fitted(fm), col.line="black")
    },
    xlab="Standardized age", ylab="Height (cm)"
)
# ----------------------------------------------------------
# Notes
# ----------------------------------------------------------
# Transpose all rows, 't(.SD)', of columns 2:13
# d[1, t(.SD), .SDcols=2:13]
#
# Combine elements
# l <- c(l, list(dout_i[i]))
