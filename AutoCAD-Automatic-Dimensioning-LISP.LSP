(defun C:AD ( / *error* a acdoc b c d dim e fd ht i o p1 p2 p3 pc pm rad sd space ss u opt isLine userChoice)
  (setq acDoc (vla-get-activedocument (vlax-get-acad-object))
        space (vlax-get acDoc (if (= (getvar 'cvport) 1) 'paperspace 'modelspace))
        dim (getvar 'dimstyle)
        ht  (* 1.0 (getvar 'dimtxt) (if (= 0 (getvar 'dimanno)) (getvar 'dimscale) (/ 1.0 (getvar 'cannoscalevalue))))
  )

  (vla-startundomark acDoc)

  (defun *error* (msg)
    (and
      msg
      (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
      (princ (strcat "\nADError: " msg))
    )
    (vla-endundomark acDoc)
    (princ)
  )

  ;; Prompt user for mode selection
  (initget "Original PickPoints")
  (setq userChoice (getkword "\nChoose method: [Original/PickPoints] <Original>: "))

  (cond
    ;; Original automatic dimensioning logic
    ((or (null userChoice) (equal userChoice "Original"))
      (setq opt (mapcar
                  '(lambda (a b)
                     (cond
                       ((getdictvar "AD_options" a))
                       ((setdictvar "AD_options" a b))
                     )
                   )
                  '("Linear" "Arc")
                  '("b0" "c0")
      ))

      (initget "Options")
      (if
        (eq (getkword "\nPress enter to continue or [Options]: ") "Options")
        (setq opt (AD_options opt))
      )

      (if
        (setq ss (ssget '((0 . "LWPOLYLINE,LINE,ARC"))))
        (repeat (setq i (sslength ss))
          (setq e (ssname ss (setq i (1- i)))
                o (vlax-ename->vla-object e)
                a (vlax-curve-getstartparam e)
                c (vlax-curve-getendparam   e)
                b nil
                isLine (wcmatch (vla-get-Objectname o) "AcDbLine,AcDbArc")
          )
          (while (<= (setq b (if isLine (if b (1+ b) c) (1+ a))) c)
            (setq p1 (vlax-curve-getpointatparam e a)
                  p2 (vlax-curve-getpointatparam e b)
                  u  (angle p1 p2)
                  pm (vlax-curve-getpointatparam e (/ (+ a b) 2.0))
                  sd (vlax-curve-getsecondderiv  e (/ (+ a b) 2.0))
                  rad (distance '(0 0 0) sd)
                  d  (cond (isLine) ((not (minusp (vla-getbulge o a)))) )
                  pc (mapcar (if d '+ '-) pm sd)
                  p3 (if
                       (or (equal rad 0.0 1e-8) (eq (cadr opt) "c2"))
                       (if
                         (eq (car opt) "b0")
                         (polar pm (+ (atan (/ (sin u) (cos u))) (/ pi 2.0)) ht)
                         (polar pm (- (atan (/ (sin u) (cos u))) (/ pi 2.0)) (* 1.75 ht))
                       )
                       (if
                         (eq (cadr opt) "c0")
                         (polar pm (angle pm pc) (if (<= 1e-4 (angle pc pm) pi) (* 1.75 ht) ht))
                         (polar pm (angle pc pm) (if (<= 1e-4 (angle pc pm) pi) ht (* 1.75 ht)))
                       )
                     )
                   )
            (if
              (equal rad 0.0 1e-8)
              (vla-adddimaligned space (vlax-3d-point p1) (vlax-3d-point p2) (vlax-3d-point p3))
              (vla-adddimarc space (vlax-3d-point pc) (vlax-3d-point p1) (vlax-3d-point p2) (vlax-3d-point p3))
            )
            (setq a (1+ a))
          )
        )
      )
    )

    ;; Manual point selection mode
    ((equal userChoice "PickPoints")
      (setq pickedPoints '()) ;; Initialize an empty list

      ;; Collect multiple pairs of points
      (while (setq p1 (getpoint "\nSelect first point (or Enter to finish): "))
        (setq p2 (getpoint "\nSelect second point: "))
        (if p2
          (setq pickedPoints (cons (list p1 p2) pickedPoints)) ;; Store pair
        )
      )

      ;; Add dimensions after user presses Enter
      (foreach pair pickedPoints
        (setq p1 (car pair) p2 (cadr pair))
        (vla-adddimaligned space 
          (vlax-3d-point p1) 
          (vlax-3d-point p2) 
          (vlax-3d-point (polar p1 (angle p1 p2) ht))
        )
      )
    )
  )

  (vla-endundomark acDoc)
  (princ)
)

(defun AD_options (old / a1 a2 b1 b2 dcl dcl_id file r)
  (setq
    a1 (car old)
    a2 (cadr old)
    dcl (open (setq file (vl-filename-mktemp "AD" (getvar 'dwgprefix) ".dcl")) "w")
  )
  (write-line
    "AD: dialog { label = \"Dimension Polyline Options\" ;
    : boxed_radio_column { label = \"Linear dimension position\" ; key = \"a1\";
    : radio_button { label = \"Above line\" ; key = \"b0\";}
    : radio_button { label = \"Below line\" ; key = \"b1\";} }
    : boxed_radio_column { label = \"Arc dimension position\" ; key = \"a2\" ;
    : radio_button { label = \"Inside arc\" ; key = \"c0\";}
    : radio_button { label = \"Outside arc\" ; key = \"c1\";}
    : radio_button { label = \"As for lines\" ; key = \"c2\";} }
    ok_cancel ;}"
    dcl)
  (close dcl)
  (if
    (< 0 (setq dcl_id (load_dialog file)))
    (if
      (new_dialog "AD" dcl_id)
      (progn
        (action_tile "a1" "(setq b1 $value)")
        (action_tile "a2" "(setq b2 $value)")
        (set_tile "a1" (setq b1 a1))
        (set_tile "a2" (setq b2 a2))
        (setq r (start_dialog))
        (unload_dialog dcl_id)
      )
    )
  )
  (if (findfile file) (vl-file-delete file))
  (if
    (= r 1)
    (mapcar 'setdictvar 
      '("AD_options" "AD_options")
      '("Linear" "Arc")
      (list b1 b2)
    )
    (list a1 a2)
  )
)

(defun getdictvar (dict var / dict_ename)
  (if
    (setq dict_ename (cdr (assoc -1 (dictsearch (namedobjdict) dict))))
    (cdr (assoc 1 (dictsearch dict_ename var)))
  )
)

(defun setdictvar (dict var val / dict_name record)
  (or
    (setq dict_ename (cdr (assoc -1 (dictsearch (namedobjdict) dict))))
    (setq dict_ename (dictadd (namedobjdict) dict (entmakex '((0 . "DICTIONARY") (100 . "AcDbDictionary")))))
  )
  (if
    (setq record (dictsearch dict_ename var))
    (entmod (subst (cons 1 val) (assoc 1 record) record))
    (dictadd
      dict_ename
      var
      (entmakex
        (list
          '(0 . "DICTIONARYVAR")
          '(100 . "DictionaryVariables")
          '(280 . 0)
          (cons 1 val)
        )
      )
    )
  )
  val
)
