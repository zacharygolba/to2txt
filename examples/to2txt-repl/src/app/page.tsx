"use client";

import React, { JSX, useState } from "react";
import clsx from "clsx";
import Link from "next/link";

import { Input } from "@playground/components/input";
import { Output } from "@playground/components/output";
import { anonymousPro, inter } from "@playground/styles/font";
import styles from "@playground/styles/page.module.css";

export default function Home(): JSX.Element {
  const [tasks, setTasks] = useState("");

  return (
    <div className="flex flex-col absolute top-2 right-2 bottom-2 left-2 overflow-hidden rounded window">
      <main className="grid h-full w-full grid-rows-2 gap-5 overflow-hidden md:grid-cols-2 md:grid-rows-1">
        <section className={styles.pane}>
          <header
            className={clsx(
              "flex-shrink-0 h-16 px-4.5 text-2xl leading-[4rem] select-none whitespace-nowrap",
              anonymousPro.className,
            )}
          >
            <Link href="https://crates.io/crates/to2txt" target="_blank">
              to2txt repl
            </Link>
          </header>
          <div className={styles.inputContainer}>
            <Input
              className={clsx(
                "min-h-full w-full text-sm text-gray-200 px-4.5 leading-relaxed",
                inter.className,
              )}
              onChange={setTasks}
              value={tasks}
            />
          </div>
        </section>
        <section
          className={clsx(
            styles.pane,
            "px-4.5 overflow-x-hidden overflow-y-auto md:px-0 md:py-4",
          )}
        >
          <Output
            className={clsx("text-sm", anonymousPro.className)}
            value={tasks}
          />
        </section>
      </main>
    </div>
  );
}
