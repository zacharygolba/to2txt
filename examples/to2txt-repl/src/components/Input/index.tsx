"use client";

import React, { JSX } from "react";
import clsx from "clsx";

export interface InputProps {
  className?: string;
  value?: string;

  onChange?(value: string): void;
}

export default function Input(props: InputProps): JSX.Element {
  return (
    <textarea
      autoCapitalize="none"
      spellCheck={false}
      placeholder={`(A) feed the tomato plants @garden +home`}
      className={clsx(
        "outline-none resize-none placeholder:italic",
        props.className,
      )}
      onInput={({ currentTarget }) => props.onChange?.(currentTarget.value)}
      value={props.value}
    />
  );
}
