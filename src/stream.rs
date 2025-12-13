pub trait Stream<T> {
    /**
     * Initializes the stream with a value
     * starts the pointer at the first character in the string
     */
    fn new(value: String) -> Self;
    /**
     * Provides an option containing what the following character is without consuming it
     */
    fn peek(&self) -> Option<T>;
    /**
     * Provides an option containing what the following character is, consuming it
     */
    fn next(&mut self) -> Option<T>;
    // /**
    //  * Provides an option containing the current character the stream points to
    //  */
    // fn current(&self) -> Option<T>;
    /**
     * Returns true if the current character is the final character in the stream
     */
    fn is_end(&self) -> bool;
}
