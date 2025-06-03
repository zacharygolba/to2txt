"use client";

import React, { memo } from "react";
import { parse } from "@playground/parser/pkg";
import clsx from "clsx";

export interface OutputProps {
  className?: string;
  value?: string;
}

export const Output = memo(({ className, value }: OutputProps) => (
  <code
    className={clsx(
      "text-white/40 whitespace-pre-wrap leading-none",
      className
    )}
  >
    {value && parse(value)}
  </code>
));
