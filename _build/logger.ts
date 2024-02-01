class Logger {
  public stream = Bun.stdout;
  private writer = this.stream.writer();
  private dateFormatter = new Intl.DateTimeFormat("en-GB", {
    dateStyle: "short",
    timeStyle: "medium",
  });

  private write(logLevel: "INFO" | "WARN" | "ERROR" | "DEBUG", ...args: any[]) {
    const message = args
      .map((a) => (typeof a !== "string" ? JSON.stringify(a) : a))
      .join(" ");
    const timestamp = this.dateFormatter.format(new Date()); // TODO: replace with Temporal.Now.instant().toLocaleString(); once Temporal API becomes available

    this.writer.write(`[${timestamp}] [${logLevel}] ${message}\n`);
  }

  log(...args: any[]) {
    this.write("INFO", ...args);
  }

  info(...args: any[]) {
    this.write("INFO", ...args);
  }

  warn(...args: any[]) {
    this.write("WARN", ...args);
  }

  error(...args: any[]) {
    this.write("ERROR", ...args);
  }

  debug(...args: any[]) {
    this.write("DEBUG", ...args);
  }

  flush() {
    this.writer.flush();
  }
}

const logger = new Logger();

export default logger;
