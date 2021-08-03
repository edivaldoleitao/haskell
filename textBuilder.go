package main

import (
	"fmt"
	"math/rand"
	"strings"
	"sync"
	"time"
)

var buffer strings.Builder
var mutex sync.Mutex

func adicionaTexto(v string, wg *sync.WaitGroup) {

	defer mutex.Unlock()
	defer wg.Done()
	mutex.Lock()

	for i := 0; i < len(v); i++ {
		buffer.WriteByte(v[i])
		r := rand.Intn(10)
		time.Sleep(time.Duration(r) * time.Millisecond)
	}

	//t <- 1
}

func main() {

	disciplinas := [10]string{"Paradigmas de Programação\n",
		"Introdução a Programação\n",
		"Matemática Discreta\n",
		"Circuitos Digitais\n",
		"Arquitetura de Computadores\n",
		"Análise e Projeto de Sistemas\n",
		"Teste de Software\n",
		"Banco de Dados\n",
		"Estrutura de Dados\n",
		"Inteligência Artificial\n"}

	//terminal := make(chan int)
	var wg sync.WaitGroup

	for _, v := range disciplinas {
		wg.Add(1)
		go adicionaTexto(v, &wg)
	}

	wg.Wait()

	fmt.Println("O conteudo do buffer é:")
	fmt.Println(buffer.String())

}
