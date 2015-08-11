
#######################################################################
# check.sh - script to check differences between existing and generated
# source files:
# vector classes  > run-main scalation.linalgebra.bld.BldVector
# matrix traits   > run-main scalation.linalgebra.bld.BldMatri
# matrix classes  > run-main scalation.linalgebra.bld.BldMatrix

#######################################################################
# check vector classes

echo diff VectorI.scalaa ../VectorI.scala
diff VectorI.scalaa ../VectorI.scala

echo diff VectorL.scalaa ../VectorL.scala
diff VectorL.scalaa ../VectorL.scala

echo diff VectorD.scalaa ../VectorD.scala
diff VectorD.scalaa ../VectorD.scala

echo diff VectorC.scalaa ../VectorC.scala
diff VectorC.scalaa ../VectorC.scala

echo diff VectorR.scalaa ../VectorR.scala
diff VectorR.scalaa ../VectorR.scala

#######################################################################
# check matrix traits

echo diff MatriI.scalaa ../MatriI.scala
diff MatriI.scalaa ../MatriI.scala

echo diff MatriL.scalaa ../MatriL.scala
diff MatriL.scalaa ../MatriL.scala

echo diff MatriD.scalaa ../MatriD.scala
diff MatriD.scalaa ../MatriD.scala

echo diff MatriC.scalaa ../MatriC.scala
diff MatriC.scalaa ../MatriC.scala

echo diff MatriR.scalaa ../MatriR.scala
diff MatriR.scalaa ../MatriR.scala

#######################################################################
# check matrix classes

echo diff MatrixI.scalaa ../MatrixI.scala
diff MatrixI.scalaa ../MatrixI.scala

echo diff MatrixL.scalaa ../MatrixL.scala
diff MatrixL.scalaa ../MatrixL.scala

echo diff MatrixD.scalaa ../MatrixD.scala
diff MatrixD.scalaa ../MatrixD.scala

echo diff MatrixC.scalaa ../MatrixC.scala
diff MatrixC.scalaa ../MatrixC.scala

echo diff MatrixR.scalaa ../MatrixR.scala
diff MatrixR.scalaa ../MatrixR.scala

