import React, { JSX, useEffect, useState } from "react";
import clsx from "clsx";

export interface OutputProps {
  className?: string;
  value?: string;
}

export function Output(props: OutputProps): JSX.Element {
  const [parser, setParser] = useState<{ parse(input: string): string }>();
  const [error, setError] = useState();

  useEffect(() => {
    import("../../wasm/pkg").then(setParser, setError);
  }, []);

  if (error) {
    throw error;
  }

  return (
    <code
      className={clsx(
        "text-white/50 whitespace-pre-wrap leading-0",
        props.className,
      )}
    >
      {parser && props.value && parser.parse(props.value)}
    </code>
  );
}
