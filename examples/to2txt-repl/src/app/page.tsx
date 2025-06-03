"use client";

import React, { JSX, useState } from "react";
import clsx from "clsx";
import Link from "next/link";

import { Input } from "@playground/components/input";
import { Output } from "@playground/components/output";
import { anonymousPro, inter } from "@playground/styles/font";

export default function Home(): JSX.Element {
  const [tasks, setTasks] = useState("");

  return (
    <div className="flex flex-col absolute top-2 right-2 bottom-2 left-2 overflow-hidden rounded window">
      <main className="grid grid-cols-1 md:grid-cols-2 gap-[20px] h-full overflow-hidden">
        <section className="flex flex-col h-1/2 md:h-full md:border-r md:border-[#383842] border-dashed overflow-hidden">
          <header
            className={clsx(
              "flex-shrink-0 h-16 px-4.5 text-2xl leading-[4rem] select-none whitespace-nowrap",
              anonymousPro.className
            )}
          >
            <Link href="https://crates.io/crates/to2txt" target="_blank">
              to2txt repl
            </Link>
          </header>
          <div className="h-full overflow-hidden">
            <Input
              className={clsx(
                "min-h-full w-full text-sm text-gray-200 leading-loose px-4.5",
                inter.className
              )}
              onChange={setTasks}
              value={tasks}
            />
          </div>
        </section>
        <section className="h-1/2 md:h-full py-4 overflow-x-hidden overflow-y-auto">
          <Output
            className={clsx("text-sm", anonymousPro.className)}
            value={tasks}
          />
        </section>
      </main>
    </div>
  );
}
