package main

import (
	"fmt"
	"time"
)

type Barista struct {
	name   string
	orders chan string
	drinks chan string
}

func (b Barista) makeDrink(name string) {
	fmt.Println(b.name, "fazendo", name)
	time.Sleep(time.Second)
}

func (b Barista) waitForOrders() {
	for {
		fmt.Println(b.name, "aguardando pedidos")
		drink := <-b.orders
		b.makeDrink(drink)
		fmt.Println(b.name, "enviando", drink)
		b.drinks <- drink
	}
}

func NewBarista(name string, orders chan string, drinks chan string) Barista {
	b := Barista{name, orders, drinks}
	go b.waitForOrders()
	return b
}

type Customer struct {
	name   string
	orders chan string
	drinks chan string
}

func (c Customer) collectDrink() {
	fmt.Println(c.name, "esperando bebida")
	drink := <-c.drinks
	fmt.Println(c.name, "recebida", drink)
}

func (c Customer) order(drink string) {
	fmt.Println(c.name, "pediu um", drink)
	c.orders <- drink
	c.collectDrink()
}

func NewCustomer(name string, orders chan string, drinks chan string) Customer {
	c := Customer{name, orders, drinks}
	return c
}

func main() {
	orders := make(chan string)
	drinks := make(chan string)

	NewBarista("Bob", orders, drinks)
	c1 := NewCustomer("Estela", orders, drinks)
	c2 := NewCustomer("Mauricio", orders, drinks)
	c1.order("Filter")
	c2.order("Flat white")
	time.Sleep(time.Second * 10)
}
