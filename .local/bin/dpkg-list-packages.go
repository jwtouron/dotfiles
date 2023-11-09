//usr/bin/env go run "$0"; exit "$?"

// List packages installed by dpkg, which aren't dependend upon by other packages.

package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"os/exec"
	"slices"
	"strings"
	"time"
)

type packageInfo struct {
	name string
	installTime time.Time
	cmd *exec.Cmd
}

func processLogFile(path string, packages *map[string]*packageInfo) {
	file, err := os.Open(path)
	if err != nil {
		log.Fatalln(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " ")
		if parts[2] == "install" {
			pkgName := strings.Split(parts[3], ":")[0]
			installTime, err := time.Parse(time.DateTime, parts[0] + " " + parts[1])
			if err != nil {
				log.Fatalln(err)
			}
			pi := &packageInfo{
				name: pkgName,
				installTime: installTime,
				cmd: nil,
			}
			(*packages)[pkgName] = pi
		} else if parts[2] == "remove" {
			pkgName := strings.Split(parts[3], ":")[0]
			delete(*packages, pkgName)
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatalln(err)
	}
}

func main() {
	entries, err := os.ReadDir("/var/log")
	if err != nil {
		log.Fatal(err)
	}

	slices.SortFunc(entries, func(e1, e2 os.DirEntry) int {
		fi1, err1 := e1.Info()
		fi2, err2 := e2.Info()
		if err1 != nil || err2 != nil {
			log.Fatalln(err1, err2)
		}
		return fi1.ModTime().Compare(fi2.ModTime())
	})

	packages := make(map[string]*packageInfo)

	for _, entry := range entries {
		if strings.HasPrefix(entry.Name(), "dpkg.log") {
			processLogFile("/var/log/" + entry.Name(), &packages)
		}
	}

	cmds := make([]*packageInfo, 0, 1000)

	for _, pi := range packages {
		idx, _ := slices.BinarySearchFunc(cmds, pi, func(pi1, pi2 *packageInfo) int {
			return pi1.installTime.Compare(pi2.installTime)
		})
		cmds = slices.Insert(cmds, idx, pi)
	}

	for _, pi := range cmds {
		pi.cmd = exec.Command(
			"sh",
			"-c",
			"grep -q '^  Depends:' <(apt rdepends --installed " + pi.name + " 2>/dev/null)")
		if err := pi.cmd.Start(); err != nil {
			log.Fatalln(err)
		}
	}

	for _, pi := range cmds {
		err = pi.cmd.Wait()
		if err != nil {
			fmt.Println(pi.name)
		}
	}
}
