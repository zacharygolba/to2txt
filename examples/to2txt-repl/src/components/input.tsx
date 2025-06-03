"use client";

import React, { JSX, useMemo } from "react";
import clsx from "clsx";

export interface InputProps {
  className?: string;
  value?: string;

  onChange?(value: string): void;
}

export function Input(props: InputProps): JSX.Element {
  return (
    <textarea
      spellCheck={false}
      placeholder={`(A) feed the tomato plants @garden +home`}
      className={clsx(
        "bg-transparent border-none text-base leading-6 outline-none resize-none font-sans placeholder:italic",
        props.className
      )}
      onChange={(e) => props.onChange?.(e.currentTarget.value)}
      value={props.value}
    />
  );
}
