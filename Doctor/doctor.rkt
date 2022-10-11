; заготовка "Доктора". Январь 2022
#lang scheme/base
; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
      (printf "(Hello, ~a!)\n" name)
      (print '(what seems to be the trouble?))
      (doctor-driver-loop-v1 name)
)

; Упражнение 5: стоп-слово + многопользовательность
(define (visit-doctor-v1 stopw clientsnum)
      (let loop ((count clientsnum))
            (if (= count 0)
                  (println '(time to go home...))
                  (let ((name (ask-patient-name)))
                        (if (equal? name stopw)
                              (println '(time to go home...))
                              (begin
                                    (printf "(Hello, ~a!)\n" name)
                                    (print '(what seems to be the trouble?))
                                    (doctor-driver-loop-v1 name)
                                    (loop (sub1 count))
                              )
                        )
                  )
            )
      )
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
      (newline)
      (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
      (let ((user-response (read)))
            (cond 
                  ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
                        (printf "(Goodbye, ~a!)\n" name)
                        (print '(see you next week))
                  )
                  (else 
                        (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                        (doctor-driver-loop name)
                  )
            )
      )
)

; Упражнение 4
; Добавление истории
(define (doctor-driver-loop-v1 name)
      (let for-history ((history '()))
            (newline)
            (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
            (let ((user-response (read)))
                  (cond 
                        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
                              (printf "(Goodbye, ~a!)\n" name)
                              (println '(see you next week))
                        )
                        (else 
                              (print (reply-v2 user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                              (for-history (cons user-response history))
                        )
                  )
            )
      )
)


; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
      (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
            ((0) (qualifier-answer user-response)) ; 1й способ
            ((1) (hedge-answer))  ; 2й способ
      )
)

; Упражнение 4 
; 3 стратегия генерации ответа
(define (reply-v1 user-response history)
      (case (random (if (null? history) 2 3)) ; с равной вероятностью выбирается один из двух способов построения ответа
            ((0) (qualifier-answer user-response)) ; 1й способ
            ((1) (hedge-answer))  ; 2й способ
            ((2) (history-answer history))
      )
)

; Упражнение 6
; 4 стратегия генерации ответа
(define (reply-v2 user-response history)
      (case (random (if (null? history) 1 0) (if (key-contains? (keys->list list-answers) user-response) 4 3)) ; с равной вероятностью выбирается один из двух способов построения ответа
            ((0) (history-answer history))
            ((1) (qualifier-answer user-response)) ; 1й способ
            ((2) (hedge-answer))  ; 2й способ
            ((3) (key-answer user-response list-answers))
      )
)

; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
      (append 
            (pick-random '(
                        (you seem to think that)
                        (you feel that)
                        (why do you believe that)
                        (why do you say that)
                        ; Упражнение 1
                        (what makes you think that)
                        (do you really think that)
                        (you want to say that)
                  )
            )
            (change-person user-response)
      )
 )


; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
      (pick-random '(
                  (please go on)
                  (many people have the same sorts of feelings)
                  (many of my patients have told me the same thing)
                  (please continue)
                  ;Упражнение 1
                  (please do not stop)
                  (this problem has not affected you alone)
                  (calm down and carry on)
            )
      )
)                                         

; Упражниние 4
; 3й способ генерации ответной реплики -- использования истории
(define (history-answer history)
      (append 
            '(earlier you said that)
            (change-person (pick-random history))
      )
)

; 4й споcоб генерации ответной реплики - ключевые слова
(define (key-answer user-response keys)
      ; Выбор ключевого слова
      (let ((keyword (pick-random (keyword-list keys user-response))))
            (let ((answer (pick-random (get-answers keyword keys))))
                  (many-replace-v3 (list (list '* keyword)) answer)
            )
      )
)



;************************ структура данных для ключевых слов 6 упражнение ****************************
(define list-answers '(
      (
            (depressed suicide exams university)
            (
                  (when you feel depressed go out for ice cream)
                  (depression is a disease that can be treated)
                  (during * you shold relax)
                  (try not to waste your time)
            )
      )
      (
            (mother father parents brother sister uncle aunt grandma grandpa)
            (
                  (tell me more about your * , i want to know all about your *)
                  (why do you feel that way about your * ?)
                  (how many relatives do you have)
                  (what is your relationship with *)
            )
      )
      (
            (university scheme lections practicum dl)
            (
                  (your education is important)
                  (how much time do you spend to education ?)
                  (how useful it is to go to * ?)
                  (do you like * ?)
            )
      )
      (
            (scheme lisp scala)
            (
                  (functional programming is a most attractive programming paradigm isnt it ?)
                  (* rules whole world !)
                  (brackets is fin isnt it ?)
                  (try to use imperative programming languages)
            )
      )
      (
            (c++ java c#)
            (
                  (object-oriented programming is a prettiest programming paradigm ever isnt it ?)
                  (* is the best one !)
                  (tell me more about *)
                  (* is one of the most popular programming languages)
            )
      )
      (
            (breakfast lunch dinner supper food eat)
            (
                  (you should eat regularry)
                  (have you already had *)
                  (healthy food is the key to health)
                  (eating 3 times a day is the best thing)
            )
      )
      (
            (ussr usa politics ukraine war)
            (
                  (politics only brings misery)
                  (don't think about *)
                  (thinking about * in your free time leads to depression)
                  (how much time do you spend thinking about it)
            )
      ))
)

(define (get-answers keyword keys)
      (foldl 
            (lambda (x y)
                  (if (memq keyword (car x))
                        (append y (cadr x))
                        y
                  )    
            )
            '()
            keys
      )
)

; Списко всех ключевых слов
(define (keyword-list keys user-response)
      (let ((lst (keys->list keys)))
            (filter (lambda(x) (memq x lst)) user-response)
      )
)

; Свернуть все ключи с ответами только в ключи, встречающие каждый по 1 разу
(define (keys->list keys)
      (let loop ((lst keys) (res '()))
            (if (null? lst) 
                  res
                  (loop 
                        (cdr lst) 
                        (append res (filter (lambda(x) (not (memq x res))) (caar lst)))
                  )
            )
      )
)

; содержиться ли ключ в ответе?
(define (key-contains? list-keys user-response)
      (call/cc
            (lambda (cc-exit)
                  (foldl 
                        (lambda (x y) (foldl
                              (lambda (w z) (if (equal? x w)
                                    (cc-exit #t)
                                    z
                              ))
                              #f
                              user-response
                        ))
                        #f
                        list-keys
                  )
            )
      )  
)

;************************ повышение уровня абстракции 1 упражнение ***********************************

; Упражние 1 - повышение уровня абстракции
; Если существует слово которое надо заменить - получим его замену или #f 
(define (contain-first? lst str)
      (call/cc
            (lambda (cc-exit)
                  (foldl 
                        (lambda (x y) (if (memq str (car x)) 
                              (cc-exit (cdr x))
                              y
                        ))
                        #f
                        lst
                  )
            )
      )  
)
; структура нового списка: '(((<замена1> <замена2>) <на что заменить>) ...)
(define (contain-second? lst str)
      (call/cc
            (lambda (cc-exit)
                  (foldl 
                        (lambda (x y) (if (equal? (cdr x) str) 
                              (cc-exit #t)
                              y
                        ))
                        #f
                        lst
                  )
            )
      )
)
; Построение списка нового вида по старому списку (конструктор)
(define (make-replacement replacement) 
      (let loop ((lst replacement) (res '()))
            (cond 
                  ((null? lst) 
                        res
                  )
                  ((contain-second? res (cdar lst)) 
                        (loop 
                              (cdr lst) 
                              (map 
                                    (lambda(x) 
                                          (if (equal? (cdar lst) (cdr x))
                                                (cons (cons (caar lst) (car x)) (cdr x))
                                                x
                                          )
                                    ) 
                                    res
                              )
                        )
                  )
                  (else 
                        (loop 
                              (cdr lst) 
                              (cons (cons (list (caar lst)) (cdar lst)) res)
                        )
                  )
            )
      )      
)
;*****************************************************************************************************

; замена лица во фразе
(define (change-person phrase)
      (many-replace-v3 '(
                  (am are)
                  (are am)
                  (i you)
                  (me you)
                  (mine yours)
                  (my your)
                  (myself yourself)
                  (you i)
                  (your my)
                  (yours mine)
                  (yourself myself)
                  (we you)
                  (us you)
                  (our your)
                  (ours yours)
                  (ourselves yourselves)
                  (yourselves ourselves)
                  (shall will)
            )
            phrase
      )
 )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
      (cond 
            ((null? lst) 
                  lst
            )
            (else 
                  (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                        (cons 
                              (if pat-rep 
                                    (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                    (car lst) ; иначе в начале ответа помещается начало списка без изменений
                              )
                              (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                  )
            )
      )
)
;Упражнение 1
(define (many-replace-v1 replacement-pairs lst)
      (let ((new (make-replacement replacement-pairs)))
            (let loop ((lst lst)) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                  (cond
                        ((null? lst) 
                              lst)
                        (else 
                              (let ((pat-rep (contain-first? new (car lst))))
                                    (cons 
                                          (if pat-rep 
                                                (car pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                                          )
                                          (loop (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                                    )
                              )
                        )
                  )
            )
      )
)

; Упражнение 2
(define (many-replace-v2 replacement-pairs lst)
      (let ((new (make-replacement replacement-pairs)))
            (let loop ((lst lst) (res '()))
                  (if (null? lst) 
                        (reverse res)
                        (let ((pat-rep (contain-first? new (car lst))))
                              (loop 
                                    (cdr lst) 
                                    (cons 
                                          (if pat-rep 
                                                (car pat-rep)
                                                (car lst)
                                          )
                                          res
                                    )
                              )
                        )
                  )
            )
      )
)

; Упражнение 3
(define (many-replace-v3 replacement-pairs lst)
      (let ((new (make-replacement replacement-pairs)))
            (map
                  (lambda (x) 
                        (let ((pat-rep (contain-first? new x)))
                              (if pat-rep
                                    (car pat-rep)
                                    x
                              )
                        )
                  )
                  lst
            )
      )
)

; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
      (let ((length (vector-length vctr)))
            (let loop ((i 0) (result init))
                  (if (= i length) 
                  result
                  (loop (add1 i) (f i result (vector-ref vctr i)))
                  )
            )
      )
)

; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
      (let ((length (vector-length vctr)))
            (let loop ((i (sub1 length)) (result init))
                  (if (= i -1) 
                  result
                  (loop (sub1 i) (f i result (vector-ref vctr i)))
                  )
            )
    )
)

(define (ask-patient-name)
      (begin
            (println '(next!))
            (println '(who are you?))
            (print '**)
            (car (read))
      ) 
)

; случайный выбор одного из элементов непустого списка lst
(define (pick-random lst)
      (list-ref lst (random (length lst)))
)

(visit-doctor-v1 'quit 3)
