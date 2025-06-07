"use client";

import React, { JSX, useState } from "react";
import clsx from "clsx";
import Link from "next/link";

import Input from "@repl/components/Input";
import Output from "@repl/components/Output";
import { anonymousPro, inter } from "@repl/styles/font";
import styles from "@repl/styles/page.module.css";

export default function Home(): JSX.Element {
  const [tasks, setTasks] = useState("");

  return (
    <div className={styles.viewport}>
      <div className={styles.grid}>
        <section className={clsx(styles.pane, "relative")}>
          <header className={clsx(styles.header, anonymousPro.className)}>
            <Link href="https://github.com/zacharygolba/to2txt" target="_blank">
              to2txt repl
            </Link>
          </header>
          <div className="h-full">
            <Input
              className={clsx(styles.input, inter.className)}
              onChange={setTasks}
              value={tasks}
            />
          </div>
        </section>
        <section className={clsx(styles.pane, "overflow-y-auto")}>
          <Output
            className={clsx("px-4.5 py-4 text-sm", anonymousPro.className)}
            value={tasks}
          />
        </section>
      </div>
    </div>
  );
}
