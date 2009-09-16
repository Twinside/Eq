<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >

    <xsl:output method="xml" indent="yes"/>

    <xsl:template match="/">
        <xsl:apply-templates select=".//annotation-xml[@encoding='LaTeX']" />
    </xsl:template>

    <xsl:template match="annotation-xml">
        <xsl:value-of select="." />
    </xsl:template>
</xsl:stylesheet>

