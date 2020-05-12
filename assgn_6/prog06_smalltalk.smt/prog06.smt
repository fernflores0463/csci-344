;; Name: Fernando Flores Hernandez
;; Time spent on assignment: 
;; Collaborators: 



(class XVector Object
  () ; abstract class

  ;;
  ;; Display methods
  ;;
  (method print ()
    (print '<<)
    (do: self (block (x) (print space) (print x)))
    (print space)
    (print '>>)
    self)
  (method debug () (subclassResponsibility self))

  ;;
  ;; Observer methods
  ;;
  (method isEmpty () (= (size self) 0))
  (method size () (subclassResponsibility self))
  (method at: (index) (at:ifAbsent: self index {(error: self 'index-out-of-bounds)}))

  ;; Implementation by Fernando
  (method at:ifAbsent: (index exnBlock) 
    (ifTrue: (< index 0) {(set index (+ (size self) index))})
    (ifTrue:ifFalse: (notNil index)
      {(elem: self index)}
      exnBlock)
  )



  (method elem: (index) (subclassResponsibility self))

  ;; copied from Collection implementation; uses do:
  (method occurrencesOf: (anObject) (locals temp)
    (set temp 0)
    (do: self (block (x)
       (ifTrue: (= x anObject) {(set temp (+ temp 1))})))
    temp)

  ;; copied from Collection implementation
  (method includes: (anObject) (< 0 (occurrencesOf: self anObject)))
  ;; copied from Collection implementation
  (method detect: (aBlock) 
    (detect:ifNone: self aBlock 
                            {(error: self 'no-object-detected)}
                            ))
  ;; copied from Collection implementation; uses do:
  (method detect:ifNone: (aBlock exnBlock) (locals answer searching)
    (set searching true)
    (do: self (block (x)
      (ifTrue: (and: searching {(value aBlock x)})
         {(set searching false) (set answer x)})))
    (if searching exnBlock {answer}))

  (method sum () (
    inject:into: self 0 (block (x sum) (+ x (coerce: x sum)))
    )
  )

  (method product () (
    inject:into: self 1 (block (x prod) (* x (coerce: x prod) ))
  ))

  (method min ()
    (ifTrue:ifFalse: (isEmpty self)
      {(error: self 'empty-list)}
      {(inject:into: self (at: self 0) (block (x min) (min: x min)))}))

  (method max ()
    (ifTrue:ifFalse: (isEmpty self)
      {(error: self 'empty-list)} ; true block
      {(inject:into: self (at: self 0) (block (x max) (max: x max)))} ; else block
    )
  )
  ;;
  ;; Iterator methods
  ;;
  (method do: (aBlock) 
    (locals index)
    (set index 0)
    (timesRepeat: (size self)
      { 
        (value aBlock (elem: self index)) 
        (set index (+ index 1))
      }
    )
  )
  ;; copied from Collection implementation; uses do:
  (method inject:into: (thisValue binaryBlock)
    (do: self (block (x) (set thisValue (value binaryBlock x thisValue))))
    thisValue)

  ;;
  ;; Comparison methods
  ;;
  ;;  *** STUDENT IMPLEMENTATION BEGINS ****

  (method similar: (anObject) 
    (locals ret index)
    (and: (isKindOf: anObject XVector) {
      (and: (= (size self) (size anObject)) {
            (set ret true) 
            (set index 0)
            (whileTrue: {(and: ret {
              (< index (size self))
              })}
               {(set ret (similar: (at: self index) (at: anObject index)))
               (set index (+ index 1))})
            ret})
    }))

  ;;;;;;;;;;;;;;;;;;;;;;

  (method < (anXVector) (locals ret i)
    (set ret nil) (set i 0)
    (while: {(and: (isNil ret) 
        {
          (< i (min: (size self) (size anXVector)))
        })
      }
      {(ifTrue: (< (at: self i) (at: anXVector i)) {(set ret true)})
        (ifTrue: (> (at: self i) (at: anXVector i)) {(set ret false)})
        (set i (+ i 1))
      }
    )
    (ifTrue: (isNil ret) {(set ret (< (size self) (size anXVector)))})
    ret
  )
  ;;  **** STUDENT IMPLEMENTATION ENDS *******

  ;; copied from Magnitude implementation; uses <
  (method >  (anXVector) (< anXVector self))
  (method <= (anXVector) (not (> self anXVector)))
  (method >= (anXVector) (not (< self anXVector)))
  (method min: (anXVector) (if (< self anXVector) {self} {anXVector}))
  (method max: (anXVector) (if (> self anXVector) {self} {anXVector}))

  ;;
  ;; Producer methods
  ;;
  (method + (anXVector) (withXV1:withXV2: ConcatXVector self anXVector))
  (method * (anInteger) (withXV:withN: RepeatXVector self anInteger))
  (method reverse () (withXV: ReverseXVector self))
  (method fromIndex:toIndex: (sindex eindex) (error: self 'unimplemented-fromIndex:toIndex:))
)


(class ArrayXVector XVector
  (arr)
  (class-method withArr: (arr) (withArr: (new self) arr))
  (method withArr: (thatArr) (set arr (from: Array thatArr)) self)
  (method debug ()
    (print 'ArrayXVector) (print left-paren) (print arr) (print right-paren))
  (method size () (size arr))
  (method elem: (anIndex) (at: arr anIndex))
)

(class ConcatXVector XVector
  () ; add instance variables as necessary
  (class-method withXV1:withXV2: (xv1 xv2) (error: self 'unimplemented-withXV1:withXV2:))
  (method debug () (error: self 'unimplemented-debug))
  (method size () (error: self 'unimplemented-size))
  (method elem: (index) (error: self 'unimplemented-elem:))
)

(class RepeatXVector XVector
  () ; add instance variables as necessary
  (class-method withXV:withN: (xv n) (error: self 'unimplemented-withXV:withN:))
  (method debug () (error: self 'unimplemented-debug))
  (method size () (error: self 'unimplemented-size))
  (method elem: (index) (error: self 'unimplemented-elem:))
)

(class ReverseXVector XVector
  () ; add instance variables as necessary
  (class-method withXV: (xv) (error: self 'unimplemented-withXV:))
  (method debug () (error: self 'unimplemented-debug))
  (method size () (error: self 'unimplemented-size))
  (method elem: (index) (error: self 'unimplemented-elem:))
)

(class SwizzleXVector XVector
  () ; add instance variables as necessary
  (class-method withXV1:withXV2: (xv1 xv2) (error: self 'unimplemented-withXV1:withXV2:))
  (method debug () (error: self 'unimplemented-debug))
  (method size () (error: self 'unimplemented-size))
  (method elem: (index) (error: self 'unimplemented-elem:))
)

(class BlockXVector XVector
  () ; add instance variables as necessary
  (class-method withN:withBlock: (n block) (error: self 'unimplemented-withN:withBlock:))
  (method debug () (error: self 'unimplemented-debug))
  (method size () (error: self 'unimplemented-size))
  (method elem: (index) (error: self 'unimplemented-elem:))
)
