<xsl:stylesheet version = '1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
    <xsl:output method="text" />

    <!--
        The main element we're interested in is "directive" at the bottom.

        Each "directive" is an Nginx config setting that we're interested in.

        To verify output, here are a few nginx directives that expose
        different paths through this XSLT file (all in ngx_core_module.xml):

        * Single syntax with block: "http"
        * Single syntax without block: "aio"
        * Two syntaxes with block: "location"
        * Two syntaxes without block: "disable_symlinks"

        * No default: "location"
        * Default with block: "types"
        * Default without block: "aio"

        * Single context: "alias"
        * Multiple contexts: "aio"

        * Description is multi-paragraph, multi line, multi sentence,
          multi node: "alias"
        * Description is multi-paragraph, multi line, single sentence: "aio"
        * Description is multi-paragraph, single line, single sentence:
          "lingering_close"
        * Description is single-paragraph: "lingering_timeout"
    -->

    <xsl:template match="node()">
        <xsl:apply-templates select="node()"/>
    </xsl:template>

    <xsl:template match="syntax">
        <xsl:choose>
            <xsl:when test="position() = 1">
                <xsl:text>Syntax: </xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <!-- Bringing 2+ syntaxes on separate rows, and aligning with 1st row -->
                <xsl:text>\n        </xsl:text>
            </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of select="../@name" />
        <xsl:if test="node()">
            <xsl:text> </xsl:text>
            <xsl:value-of select="normalize-space()" />
            <xsl:apply-templates />
        </xsl:if>
        <xsl:choose>
            <xsl:when test="@block = 'yes'">
                <xsl:text> { ... }</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>;</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="default">
        <xsl:text>\nDefault: </xsl:text>
        <xsl:choose>
            <xsl:when test="node()">
                <xsl:value-of select="../@name" />
                <xsl:choose>
                    <xsl:when test="../syntax[@block = 'yes']">
                        <xsl:text> {</xsl:text>
                        <xsl:value-of select="normalize-space()" />
                        <xsl:text>}</xsl:text>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:text> </xsl:text>
                        <xsl:value-of select="normalize-space()" />
                        <xsl:text>;</xsl:text>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>—</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="context">
        <xsl:choose>
            <xsl:when test="position() = 1">
                <xsl:text>\nContext: </xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>, </xsl:text>
            </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of select="normalize-space()" />
    </xsl:template>

    <xsl:template match="para" mode="first-para-within-directive">
        <xsl:choose>
            <xsl:when test="count(./text()[contains(normalize-space(), '. ') or substring(normalize-space(), string-length(normalize-space())) = '.']) = 0" >
                <!-- No text node ends a sentence. Hence full-dumping the whole para -->
                <xsl:apply-templates mode="full-dump-with-smart-space" />
            </xsl:when>
            <xsl:otherwise>
                <!--
                    Some text node contains '. ' or ends in '.', hence ending
                    the first sentence. Dumping that node in node-with-first-sentence-end mode
                -->
                <xsl:apply-templates select="./text()[contains(normalize-space(), '. ') or substring(normalize-space(), string-length(normalize-space())) = '.'][1]" mode="node-with-first-sentence-end" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="node()" mode="smart-space" >
        <xsl:choose>
            <!-- No space, if next node ends seems to end the sentence and para -->
            <xsl:when test="normalize-space(following-sibling::node()) = '.'" />
            <!-- No space, if next node ends the sentence -->
            <xsl:when test="starts-with(normalize-space(following-sibling::node()), '. ')" />
            <!-- No space, if next node starts with a comma (enumerations) -->
            <xsl:when test="starts-with(normalize-space(following-sibling::node()), ',')" />
            <!-- No space, if next node ends a quote -->
            <xsl:when test="starts-with(normalize-space(following-sibling::node()), '”')" />
            <!-- No space, if this node ends by starting a quote -->
            <xsl:when test="substring(normalize-space(), string-length(normalize-space())) = '“'" />
            <!-- No space, if next node paren group -->
            <xsl:when test="starts-with(normalize-space(following-sibling::node()), ')')" />
            <!-- No space, if this node starts a paren group -->
            <xsl:when test="substring(normalize-space(), string-length(normalize-space())) = '('" />
            <!-- No space, if this node has no following siblings -->
            <xsl:when test="count(following-sibling::node()) = 0" />
            <!-- No space, if this node has only one following sibling, which is a whitespace text nodes -->
            <xsl:when test="count(following-sibling::node()) = 1 and normalize-space(following-sibling::node()) = ''" />
            <xsl:otherwise>
                <xsl:text> </xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="node()" mode="full-dump-with-smart-space" >
        <xsl:choose>
            <xsl:when test="count(./preceding-sibling::example) &gt; 0" />
            <xsl:otherwise>
                <xsl:value-of select="normalize-space()" />
                <xsl:apply-templates select="." mode="smart-space" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="list" mode="full-dump-with-smart-space" >
        <!-- Lists are pretty lengthy, and hence are omitted on purpose -->
        <xsl:text>…</xsl:text>
    </xsl:template>

    <xsl:template match="link" mode="full-dump-with-smart-space" >
        <xsl:choose>
            <xsl:when test="count(./preceding-sibling::example) &gt; 0" />
            <xsl:otherwise>
                <xsl:choose>
                    <xsl:when test="normalize-space() = ''">
                        <xsl:value-of select="./@id" />
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="normalize-space()" />
                    </xsl:otherwise>
                </xsl:choose>
                <xsl:apply-templates select="." mode="smart-space" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="literal" mode="full-dump-with-smart-space" >
        <xsl:choose>
            <xsl:when test="count(./preceding-sibling::example) &gt; 0" />
            <xsl:otherwise>
                <xsl:text>&lt;code&gt;</xsl:text>
                <xsl:value-of select="normalize-space()" />
                <xsl:text>&lt;/code&gt;</xsl:text>
                <xsl:apply-templates select="." mode="smart-space" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="linefeed-to-backslash-n">
        <xsl:param name="str" />
        <xsl:choose>
            <xsl:when test="contains($str, '&#x0A;')">
                <xsl:value-of select="substring-before($str, '&#x0A;')" />
                <xsl:if test="substring-before($str, '&#x0A;') != '' and substring-after($str, '&#x0A;') != ''">
                    <xsl:value-of select="'\n'" />
                </xsl:if>
                <xsl:call-template name="linefeed-to-backslash-n">
                    <xsl:with-param name="str" select="substring-after($str, '&#x0A;')" />
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$str" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="example" mode="full-dump-with-smart-space" >
        <xsl:choose>
            <xsl:when test="count(./preceding-sibling::example) &gt; 0" />
            <xsl:otherwise>
                <xsl:text>&lt;pre&gt;&lt;code&gt;</xsl:text>
                <xsl:call-template name="linefeed-to-backslash-n">
                    <xsl:with-param name="str" select="." />
                </xsl:call-template>
                <xsl:text>&lt;/code&gt;&lt;/pre&gt;</xsl:text>
                <xsl:apply-templates select="." mode="smart-space" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="node()" mode="node-with-first-sentence-end" >
        <xsl:apply-templates select="./preceding-sibling::node()" mode="full-dump-with-smart-space" />
        <xsl:choose>
            <xsl:when test="count(./preceding-sibling::example) &gt; 0" />
            <xsl:when test="contains(normalize-space(), '. ')">
                <xsl:value-of select="substring-before(normalize-space(), '. ')" />
                <xsl:text>.</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="normalize-space()" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="directive">
        <!-- Column #1: Full article title -->
        <xsl:value-of select="@name" />
        <xsl:text>	</xsl:text>

        <!-- Column #2: Type of article -->
        <xsl:text>A</xsl:text> <!-- "A" means "actual article" -->

        <xsl:text>										</xsl:text>

        <!-- Column #12: Abstract -->
        <xsl:text>&lt;pre&gt;&lt;code&gt;</xsl:text>
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="default"/>
        <xsl:apply-templates select="context"/>
        <xsl:text>&lt;/code&gt;&lt;/pre&gt;</xsl:text>
        <xsl:apply-templates select="para[1]" mode="first-para-within-directive"/>

        <xsl:text>	</xsl:text>

        <!-- Column #13: Url -->
        <xsl:text>http://nginx.org</xsl:text>
        <xsl:value-of select="/module/@link" />
        <xsl:text>#</xsl:text>
        <xsl:value-of select="@name" />

        <!-- Linebreak -->
        <xsl:text>
</xsl:text>
    </xsl:template>
</xsl:stylesheet>
