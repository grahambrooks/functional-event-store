(ns functional-event-store.core-test
  (:require [midje.sweet :refer :all]
            [functional-event-store.core :refer :all]))


(defmacro defaggregate
  "An aggregate entity has attributes for the current structure - how
  to create new ones and an instance state. The structure meta data
  helps build new instnaces and apply event data to the state."
  [structure]
  structure)


(defn new-customer-event
  ([name] {:event-type :new-customer
           :customer {:name name
                      :email nil}})
  ([name email]
   {:event-type :new-customer
    :name name
    :email email}))


(defn new-customer-name
  [name]
  {:event-type :new-customer-name
   :name name })

(defn new-customer-email
  [email]
  {:event-type :new-customer-email
   :email email })

(defn default-handler
  [entity event]
  (merge entity (select-keys event (keys entity))))

(defn handle-new-customer-event
  [customer event]
  (default-handler customer event))

(defn handle-new-customer-email
  [customer event]
  (default-handler customer event))

(defn handle-new-customer-name
  [customer event]
  (default-handler customer event))


(def handler-map
  {:new-customer handle-new-customer-event
   :new-customer-email handle-new-customer-email
   :new-customer-name handle-new-customer-name})

(defn handle-event
  [entity event handlers]
  (let [event-type (:event-type event)
        handler (event-type handlers)]
    (handler entity event)))


(defn hydrate-entity
  [entity events]
  (if (empty? events) entity (hydrate-entity (handle-event entity (first events) handler-map) (rest events))))

(defn add-event
  ([event] [event])
  ([store event] (concat store [event])))


(facts "about event mapping"
       (fact "Calls handler"
             (handle-event {:name nil} (new-customer-name "foo") handler-map) => {:name "foo"}))

(facts "hydration of entity"
       (fact
        (let [events [(new-customer-event "graham")
                      (new-customer-name "Graham")
                      (new-customer-email "a@b")
                      (new-customer-email "b@c")]]
          (hydrate-entity {:name nil :email nil} events) => {:name "Graham" :email "b@c"})))

(facts "event store"
       (fact "can add event to empty store"
             (add-event :event) => [:event])
       (fact "can add event occupied storeempty store"
             (add-event [:event1] :event2) => [:event1 :event2]))

(facts "event structures"
       (fact (new-customer-event "name" "email") => {:event-type :new-customer
                                                     :name "name"
                                                     :email "email"} )
       (fact (new-customer-email "a@b.com") => {:event-type :new-customer-email
                                                :email "a@b.com" }))

(facts "event processing"
       (fact (defaggregate {} => {}))
       (fact (defaggregate {:name nil} => {:name nil}))
       (fact (defaggregate {:name nil :email nil}) => {:name nil :email nil})
       (fact (new-customer-event "a" "b") => {:email "b", :name "a", :event-type :new-customer})
       
       (fact "event does not add fields"
             (let [aggregate (defaggregate {:name nil})]
               (handle-new-customer-event aggregate (new-customer-event "Graham Brooks" "graham@grahambrooks.com")) => {:name "Graham Brooks"}))
       
       (fact "event changes all event fields"
             (let [aggregate (defaggregate {:name nil :email nil})]
               (handle-new-customer-event aggregate (new-customer-event "Graham Brooks" "graham@grahambrooks.com")) => {:name "Graham Brooks" :email "graham@grahambrooks.com"}))
       
       (fact
        (let [customer {:name "foo" :email "bar"}]
          (handle-new-customer-email customer (new-customer-email "a@b.com")) => {:name "foo" :email "a@b.com"})))

