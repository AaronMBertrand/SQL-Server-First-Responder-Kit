USE [master];
GO
CREATE PROCEDURE dbo.sp_foreachdb

/*
   Aaron Bertrand
   abertrand@sqlsentry.com

   Replacement for system procedure sp_MSForEachDB, which is undocumented, unsupported, and buggy.
   More info here:
   
   https://www.mssqltips.com/sqlservertip/2201/making-a-more-reliable-and-flexible-spmsforeachdb/
*/

    @command              nvarchar(max),
    @replace_character    nchar(1) = N'?',
    @print_dbname         bit = 0,
    @print_command_only   bit = 0,
    @suppress_quotename   bit = 0,
    @system_only          bit = NULL,
    @user_only            bit = NULL,
    @name_pattern         nvarchar(300) = N'%', 
    @database_list        nvarchar(max) = NULL,
    @recovery_model_desc  nvarchar(120) = NULL,
    @compatibility_level  tinyint       = NULL,
    @state_desc           nvarchar(120) = N'ONLINE',
    @is_read_only         bit = 0,
    @is_auto_close_on     bit = NULL,
    @is_auto_shrink_on    bit = NULL,
    @is_broker_enabled    bit = NULL
AS
BEGIN
    SET NOCOUNT ON;

    DECLARE
        @sql    nvarchar(max),
        @dblist nvarchar(max),
        @db     nvarchar(300),
        @i      int;

    IF @database_list > N''
    BEGIN
        ;WITH n(n) AS 
        (
            SELECT ROW_NUMBER() OVER (ORDER BY s1.name) - 1
            FROM sys.objects AS s1 
            CROSS JOIN sys.objects AS s2
        )
        SELECT @dblist = REPLACE(REPLACE(REPLACE(x,'</x><x>',','),
        '</x>',''),'<x>','')
        FROM 
        (
            SELECT DISTINCT x = 'N''' + LTRIM(RTRIM(SUBSTRING(
            @database_list, n,
            CHARINDEX(',', @database_list + ',', n) - n))) + ''''
            FROM n WHERE n <= LEN(@database_list)
            AND SUBSTRING(',' + @database_list, n, 1) = ','
            FOR XML PATH('')
        ) AS y(x);
    END

    CREATE TABLE #x(db nvarchar(300));

    SET @sql = N'SELECT name FROM sys.databases WHERE 1=1'
        + CASE WHEN @system_only = 1 THEN 
            ' AND database_id IN (1,2,3,4)' 
            ELSE '' END
        + CASE WHEN @user_only = 1 THEN 
            ' AND database_id NOT IN (1,2,3,4)' 
            ELSE '' END
        + CASE WHEN @name_pattern <> N'%' THEN 
            ' AND name LIKE N''%' + REPLACE(@name_pattern, '''', '''''') + '%''' 
            ELSE '' END
        + CASE WHEN @dblist IS NOT NULL THEN 
            ' AND name IN (' + @dblist + ')' 
            ELSE '' END
        + CASE WHEN @recovery_model_desc IS NOT NULL THEN
            ' AND recovery_model_desc = N''' + @recovery_model_desc + ''''
            ELSE '' END
        + CASE WHEN @compatibility_level IS NOT NULL THEN
            ' AND compatibility_level = ' + RTRIM(@compatibility_level)
            ELSE '' END
        + CASE WHEN @state_desc IS NOT NULL THEN
            ' AND state_desc = N''' + @state_desc + ''''
            ELSE '' END
        + CASE WHEN @is_read_only IS NOT NULL THEN
            ' AND is_read_only = ' + RTRIM(@is_read_only)
            ELSE '' END
        + CASE WHEN @is_auto_close_on IS NOT NULL THEN
            ' AND is_auto_close_on = ' + RTRIM(@is_auto_close_on)
            ELSE '' END
        + CASE WHEN @is_auto_shrink_on IS NOT NULL THEN
            ' AND is_auto_shrink_on = ' + RTRIM(@is_auto_shrink_on)
            ELSE '' END
        + CASE WHEN @is_broker_enabled IS NOT NULL THEN
            ' AND is_broker_enabled = ' + RTRIM(@is_broker_enabled)
        ELSE '' END;

        INSERT #x EXEC sys.sp_executesql @sql;

        DECLARE c CURSOR 
            LOCAL FORWARD_ONLY STATIC READ_ONLY
            FOR SELECT CASE WHEN @suppress_quotename = 1 THEN 
                    db
                ELSE
                    QUOTENAME(db)
                END 
            FROM #x ORDER BY db;

        OPEN c;

        FETCH NEXT FROM c INTO @db;

        WHILE @@FETCH_STATUS = 0
        BEGIN
            SET @sql = REPLACE(@command, @replace_character, @db);

            IF @print_command_only = 1
            BEGIN
                PRINT '/* For ' + @db + ': */'
                + CHAR(13) + CHAR(10) + CHAR(13) + CHAR(10)
                + @sql 
                + CHAR(13) + CHAR(10) + CHAR(13) + CHAR(10);
            END
            ELSE
            BEGIN
                IF @print_dbname = 1
                BEGIN
                    PRINT '/* ' + @db + ' */';
                END

                EXEC sys.sp_executesql @sql;
            END

            FETCH NEXT FROM c INTO @db;
    END

    CLOSE c;
    DEALLOCATE c;
END
GO
