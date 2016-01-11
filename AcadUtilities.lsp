(vl-load-com)

;; Declare Global Variable

(setq AcObj    (vlax-get-acad-object)
      AcDoc    (vla-get-ActiveDocument AcObj)
      AcMSpace (vla-get-ModelSpace AcDoc)
)

;; Convert 3d point to 2d point
;; Arguments: Point3D
;; Return: Point2D

(defun AU:3dPnt->2dPnt	(3dpt)
  (list (float (car 3dpt)) (float (cadr 3dpt)))
)

;; Change degree to radian
;; Arguments: angle in degree
;; Return: angle in radian

(defun AU:Angle-d2r (dgr)
    (* pi (/ dgr 180.0))
)

;; Change radian to degree
;; Arguments: angle in radian
;; Return: angle in degree

(defun AU:Angle-r2d (rad)
    (* 180 (/ rad pi))
)

;; Insert Block Reference vlaObj
;; Arguments: insertionPoint blkName scaleX scaleY scaleZ angle layer
;; Return: BlkRefObj

(defun AU:BlkRef-Insert	(insertionPnt blkName xScale yScale zScale angl	lyr / blkRefObj)
    (setq blkRefObj (vla-InsertBlock
			AcMSpace
			(vlax-3d-point insertionPnt)
			blkName
			xScale
			yScale
			zScale
			angl
		    )
    )
    (vlax-put-property blkRefObj 'Layer lyr)
    blkRefObj
)

;; Make CIRCLE entity
;; Arguments: centerPoint radius layer
;; Return: circle entity

(defun AU:Circle-Make (centerPoint radius lyr)
    (entmake (list (cons 0 "CIRCLE")
		   (cons 10 centerPoint)
		   (cons 40 radius)
		   (cons 8 lyr)
	     )
    )
)

;; Add ELLIPSE vlaObj
;; Arguments: centerPoint majorAxis radiusRatio layer
;; Return: ellipse vlaObj

(defun AU:Ellipse-Add (centerPoint majorAxis radiusRatio lyr)
    (setq ellipseObj
	     (vla-AddEllipse
		 AcMSpace
		 (vlax-3d-point centerPoint)
		 (vlax-3d-point majorAxis)
		 radiusRatio
	     )
    )
    (vlax-put-property ellipseObj 'Layer lyr)
    ellipseObj
)


;; Make LINE entity
;; Arguments: startPoint endPoint layer
;; Return: line entity

(defun AU:Line-Make (startPoint endPoint lyr)
    (entmake (list (cons 0 "LINE")
		   (cons 10 startPoint)
		   (cons 11 endPoint)
		   (cons 8 lyr)
	     )
    )
)

;; Convert PointLst to VariantArray
;; Arguments: Point List
;; Return: Variant Array

(defun AU:List->VariantArray (pntLst / arraySpace sArray)
    (setq arraySpace
	     (vlax-make-safearray
		 vlax-vbdouble		; element type
		 (cons 0 (- (length pntLst) 1)) ; array dimension
	     )
    )
    (setq sArray (vlax-safearray-fill arraySpace pntLst))
    ;; return array variant
    (vlax-make-variant sArray)
)

;; Add LWPOLYLINE vlaObj
;; Arguments: PointList layer
;; Return: LwPolyline vlaObj

(defun AU:Lwp-Add (pntLst layer / lwpObj)
    (setq pntArray (AU:List->VariantArray
		       (apply 'append (mapcar 'AU:3dPnt->2dPnt pntLst))
		   )
	  lwpObj   (vla-AddLightWeightPolyline AcMSpace pntArray)
    )
    (vla-put-layer lwpObj layer)
    lwpObj
)

;; Make Text
;; Arguments: content point layer height color rotationAngle groupCode72 groupCode73
;; Return: text entity

(defun AU:Text-Make (content pnt lyr height color angl grpCode72 grpCode73)
    (entmake (list (cons 0 "TEXT")
		   (cons 1 content)
		   (cons 10 pnt)
		   (cons 11 pnt)
		   (cons 8 lyr)
		   (cons 40 height)
		   (cons 62 color)
		   (cons 50 angl)
		   (cons 7 "HiTD-TxtSt")
		   (cons 72 grpCode72) ;; Horizontal text justification type 
		   (cons 73 grpCode73) ;; Vertical text justification type 
	     )
    )
)
