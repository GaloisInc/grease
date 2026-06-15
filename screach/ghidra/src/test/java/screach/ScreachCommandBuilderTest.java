package screach;

import static org.junit.Assert.assertEquals;

import java.util.Collections;
import java.util.List;

import org.junit.Test;

import ghidra.test.AbstractGhidraHeadlessIntegrationTest;

public class ScreachCommandBuilderTest extends AbstractGhidraHeadlessIntegrationTest {

    @Test
    public void tokenizeEmpty() {
        assertEquals(Collections.emptyList(), ScreachCommandBuilder.tokenize(""));
        assertEquals(Collections.emptyList(), ScreachCommandBuilder.tokenize(null));
        assertEquals(Collections.emptyList(), ScreachCommandBuilder.tokenize("   "));
    }

    @Test
    public void tokenizeSingleToken() {
        assertEquals(List.of("hello"), ScreachCommandBuilder.tokenize("hello"));
    }

    @Test
    public void tokenizeMultipleTokens() {
        assertEquals(
                List.of("--foo", "bar", "baz"),
                ScreachCommandBuilder.tokenize("--foo bar baz"));
    }

    @Test
    public void tokenizeExtraWhitespace() {
        assertEquals(List.of("a", "b"), ScreachCommandBuilder.tokenize("  a   b  "));
    }

    @Test
    public void tokenizeQuotedString() {
        assertEquals(
                List.of("--flag", "a b c"),
                ScreachCommandBuilder.tokenize("--flag \"a b c\""));
    }

    @Test
    public void tokenizeEmptyQuotes() {
        assertEquals(List.of(""), ScreachCommandBuilder.tokenize("\"\""));
    }

    @Test
    public void parentDirSimple() {
        assertEquals("/home/user", ScreachCommandBuilder.parentDir("/home/user/binary"));
    }
}
