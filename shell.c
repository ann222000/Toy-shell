#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
enum TOKENS {COMORFILE, INP, OUTE, OUTB, CPIP, BACK, SEQ, LB, RB, NO};
enum {ERROR = 1000};
//command(with parametrs) or file,  input <, output >>, output >
//command pipe |, background mode &, sequentially ;, (, ), no lexeme
struct lexeme {
    int count;//counter for amount of ()
    enum TOKENS token;
    char **command;//max [256][256]
};

struct tree {
    struct lexeme *key;
    struct tree *left;
    struct tree *right;
};

struct lexeme *readtoken(int * firstsymbol) {
    int c = *firstsymbol;
    if (c == ERROR)
        return NULL;
    while (isspace(c) && (c != EOF)) {
        c = getchar();
    }
    *firstsymbol = ' ';
    if (c == EOF)
        return NULL;
    struct lexeme *nowtoken;
    nowtoken = malloc(sizeof(struct lexeme));
    nowtoken->command = NULL;
    nowtoken->count = 0;
    switch (c) {
    case ';':
        nowtoken->token = SEQ;
        return nowtoken;
    case '|':
        nowtoken->token = CPIP;
        return nowtoken;
    case '&':
        nowtoken->token = BACK;
        return nowtoken;
    case '<':
        nowtoken->token = INP;
        return nowtoken;
    case '(':
        nowtoken->token = LB;
        return nowtoken;
    case ')':
        nowtoken->token = RB;
        return nowtoken;
    case '>':
        c = getchar();
        if (c == '>') {
            *firstsymbol = ' ';
            nowtoken->token = OUTE;
        }
        else {
            *firstsymbol = c;
            nowtoken->token = OUTB;
        }
        return nowtoken;
    default:
        nowtoken->token = COMORFILE;
        for (int i = 0; i < 255; ++i) {
            nowtoken->command = realloc(nowtoken->command, (i + 2) * sizeof(char *));
            nowtoken->command[i] = malloc(256);
            nowtoken->command[i][0] = c;
            int j = 1;
            while (!isspace(c = getchar()) && (c != EOF) && (strchr("();|&<>", c) == NULL) && (j < 255)) {
                nowtoken->command[i][j] = c;
                ++j;
            }
            nowtoken->command[i][j] = '\0';
            if ((!isspace(c)) && (strchr("();|&<>", c) == NULL) && (c != EOF)) {
                fprintf(stderr, "Too long name of command, its parametrs or name of file!\n");
                exit(1);
            }
            while (isspace(c)) {
                c = getchar();
            }
            if ((c == EOF) || (strchr("();|&|<>", c) != NULL)) {
                *firstsymbol = c;
                nowtoken->command[i + 1] = NULL;
                return nowtoken;
            }
        }
        fprintf(stderr, "Too much command parametrs!\n");
        exit(1);
    }
}

struct tree *expression(int *firstsymbol, struct lexeme **nowlexeme);
struct tree *commandpipe(int *firstsymbol, struct lexeme **nowlexeme);
struct tree *inoutput(int *firstsymbol, struct lexeme **nowlexeme);
struct tree *commandread(int * firstsymbol, struct lexeme **nowlexeme);
void freedom(struct tree *commandtree);

struct tree *parser(void) {
    int firstsymbol;
    firstsymbol = ' ';
    struct lexeme *nowlexeme;
    nowlexeme = NULL;
    struct tree *alltree;
    alltree = NULL;
    alltree = expression(&firstsymbol, &nowlexeme);
    if (firstsymbol == ERROR) {
        freedom(alltree);
        exit(1);
    }
    if ((nowlexeme != NULL) || (readtoken(&firstsymbol) != NULL)) {
        freedom(alltree);
        fprintf(stderr, "Unexpected end of expression!\n");
        exit(1);
    }
    return alltree;
}

struct tree *expression(int *firstsymbol, struct lexeme **nowlexeme) {
    struct tree *left;
    struct tree *alltree;
    alltree = commandpipe(firstsymbol, nowlexeme);
    if (alltree == NULL)
        return NULL;
    struct lexeme * sign;
    if (*nowlexeme != NULL) {
        sign = *nowlexeme;
        *nowlexeme = NULL;
    }
    else
        sign = readtoken(firstsymbol);
    while ((sign != NULL) && (sign->token != RB)) {
        left = alltree;
        alltree = malloc(sizeof(struct tree));
        alltree->key = sign;
        alltree->left = left;
        if ((sign->token != SEQ) && (sign->token != BACK)) {
            fprintf(stderr, "Expected sign of operation!\n");
            *firstsymbol = ERROR;
            alltree->right = NULL;
            return alltree;
        }
        alltree->right = commandpipe(firstsymbol, nowlexeme);
        if (alltree->right == NULL)
            break;
        if (*nowlexeme != NULL) {
            sign = *nowlexeme;
            *nowlexeme = NULL;
        }
        else
            sign = readtoken(firstsymbol);
    }
    if ((sign != NULL) && (sign->token == RB))
        *nowlexeme = sign;
    return alltree;
}

struct tree *commandpipe(int *firstsymbol, struct lexeme **nowlexeme) {
    struct tree *alltree;
    alltree = inoutput(firstsymbol, nowlexeme);
    struct tree *left;
    struct lexeme *sign;
    if (*nowlexeme != NULL) {
        sign = *nowlexeme;
        *nowlexeme = NULL;
    }
    else
        sign = readtoken(firstsymbol);
    while (sign != NULL) {
        if (sign->token != CPIP) {
            *nowlexeme = sign;
            return alltree;
        }
        left = alltree;
        alltree = malloc(sizeof(struct tree));
        alltree->key = sign;
        alltree->left = left;
        alltree->right = inoutput(firstsymbol, nowlexeme);
        if (alltree->right == NULL) {
            fprintf(stderr, "Unexpected end of input!\n");
            *firstsymbol = ERROR;
            return alltree;
        }
        if (*nowlexeme != NULL) {
            sign = *nowlexeme;
            *nowlexeme = NULL;
        }
        else
            sign = readtoken(firstsymbol);
    }
    return alltree;
}

struct tree *inoutput(int *firstsymbol, struct lexeme **nowlexeme) {
    struct tree *alltree;
    alltree = commandread(firstsymbol, nowlexeme);
    struct tree *left;
    struct lexeme *sign;
    if (*nowlexeme != NULL) {
        sign = *nowlexeme;
        *nowlexeme = NULL;
    }
    else
        sign = readtoken(firstsymbol);
    while (sign != NULL) {
        if ((sign->token != OUTE) && (sign->token != OUTB) && (sign->token != INP)) {
            *nowlexeme = sign;
            return alltree;
        }
        left = alltree;
        alltree = malloc(sizeof(struct tree));
        alltree->key = sign;
        alltree->left = left;
        alltree->right = commandread(firstsymbol, nowlexeme);
        if (alltree->right == NULL) {
            fprintf(stderr, "Expected name of file!\n");
            *firstsymbol = ERROR;
            return alltree;
        }
        if (*nowlexeme != NULL) {
            sign = *nowlexeme;
            *nowlexeme = NULL;
        }
        else
            sign = readtoken(firstsymbol);
    }
    return alltree;
}

struct tree *commandread(int * firstsymbol, struct lexeme **nowlexeme) {
    struct tree *alltree;
    struct lexeme *sign;
    if (*nowlexeme != NULL) {
        sign = *nowlexeme;
        *nowlexeme = NULL;
    }
    else
        sign = readtoken(firstsymbol);
    if (sign == NULL)
        return NULL;
    if (sign->token == LB) {
        free(sign);
        alltree = expression(firstsymbol, nowlexeme);
        (alltree->key->count)++;
        if (*nowlexeme != NULL) {
            sign = *nowlexeme;
            *nowlexeme = NULL;
        }
        else
            sign = readtoken(firstsymbol);
        if ((sign == NULL) || (sign->token != RB)) {
            fprintf(stderr, "Expected closing bracket!\n");
            *firstsymbol = ERROR;
        }
        free(sign);
        return alltree;
    }
    alltree = malloc(sizeof(struct tree));
    alltree->key = sign;
    alltree->left = NULL;
    alltree->right = NULL;
    if (sign->token != COMORFILE) {
        fprintf(stderr, "Expected command or name of file!\n");
        *firstsymbol = ERROR;
    }
    return alltree;
}

pid_t executer(struct tree *alltree, struct tree *tree, int ***fd, int fd0, int fd1, int num) {
    pid_t pid, pid1;
    int status, n = 0;
    if (tree->key->token == SEQ) {
        if (tree->key->count > 0) {
            if ((pid = fork()) == 0) {
                if ((fd0 == -1) || (fd1 == -1)) {
                    n = 0;
                    while (fd[0][n] != NULL) {
                        close(fd[0][n][1]);
                        close(fd[0][n][0]);
                        free(fd[0][n]);
                        n++;
                    }
                    free(fd[0]);
                    freedom(alltree);
                    fprintf(stderr, "It is not avalable to open a file!\n");
                    exit (1);
                }
                if (fd0 != 0) {
                    dup2(fd0, 0);
                    close(fd0);
                }
                if (fd1 != 1) {
                    dup2(fd1, 1);
                    close(fd1);
                }
                n = 0;
                if ((num == 1) && (fd0 == 0)) {
                    while (fd[0][n] != NULL)
                        n++;
                    if (n > 0)
                        dup2(fd[0][n - 1][1], 1);
                }
                n = 0;
                if ((num == 0) && ((fd1 == 1) || (fd0 == 0))) {
                    while (fd[0][n] != NULL)
                        n++;
                    if ((n > 1) && (fd1 == 1))
                        dup2(fd[0][n - 2][1], 1);
                    if ((n >= 1) && (fd0 == 0))
                        dup2(fd[0][n - 1][0], 0);
                }
                n = 0;
                while (fd[0][n] != NULL) {
                    close(fd[0][n][1]);
                    close(fd[0][n][0]);
                    free(fd[0][n]);
                    n++;
                }
                free(fd[0]);
                fd[0] = malloc(sizeof(int *));
                fd[0][0] = NULL;
                pid1 = executer(alltree, tree->left, fd, 0, 1, 2);
                if ((pid1 != 0) || (tree->right == NULL))
                    waitpid(pid1, &status, 0);
                if (tree->right != NULL)
                    executer(alltree, tree->right, fd, 0, 1, 2);
                while (wait(&status) != -1) {
                }
                free(fd[0]);
                freedom(alltree);
                if (WIFEXITED(status))
                    exit (WEXITSTATUS(status));
                else
                    exit (128 + WTERMSIG(status));
            }
            return pid;
        }
        pid1 = executer(alltree, tree->left, fd, fd0, fd1, num);
        if (tree->right == NULL)
            return pid1;
        while (wait(NULL) != - 1){
        }
        return executer(alltree, tree->right, fd, fd0, fd1, num);
    }
    if (tree->key->token == INP) {
        if (fd0 == 0)
            fd0 = open(tree->right->key->command[0], O_RDONLY);
        return (executer(alltree, tree->left, fd, fd0, fd1, num));
    }
    if (tree->key->token == OUTB) {
        if (fd1 == 1)
            fd1 = open(tree->right->key->command[0], O_WRONLY | O_CREAT | O_TRUNC, 0666);
        return (executer(alltree, tree->left, fd, fd0, fd1, num));
    }
    if (tree->key->token == OUTE) {
        if (fd1 == 1)
            fd1 = open(tree->right->key->command[0], O_WRONLY | O_CREAT | O_APPEND, 0666);
        return (executer(alltree, tree->left, fd, fd0, fd1, num));
    }
    if (tree->key->token == BACK) {
        if (tree->key->count > 0) {
            if ((pid = fork()) == 0) {
                if ((fd0 == -1) || (fd1 == -1)) {
                    n = 0;
                    while (fd[0][n] != NULL) {
                        close(fd[0][n][1]);
                        close(fd[0][n][0]);
                        free(fd[0][n]);
                        n++;
                    }
                    free(fd[0]);
                    freedom(alltree);
                    fprintf(stderr, "It is not avalable to open a file!\n");
                    exit (1);
                }
                if (fd0 != 0) {
                    dup2(fd0, 0);
                    close(fd0);
                }
                if (fd1 != 1) {
                    dup2(fd1, 1);
                    close(fd1);
                }
                n = 0;
                if ((num == 1) && (fd0 == 0)) {
                    while (fd[0][n] != NULL)
                        n++;
                    if (n > 0)
                        dup2(fd[0][n - 1][1], 1);
                }
                n = 0;
                if ((num == 0) && ((fd1 == 1) || (fd0 == 0))) {
                    while (fd[0][n] != NULL)
                        n++;
                    if ((n > 1) && (fd1 == 1))
                        dup2(fd[0][n - 2][1], 1);
                    if ((n >= 1) && (fd0 == 0))
                        dup2(fd[0][n - 1][0], 0);
                }
                n = 0;
                while (fd[0][n] != NULL) {
                    close(fd[0][n][1]);
                    close(fd[0][n][0]);
                    free(fd[0][n]);
                    n++;
                }
                free(fd[0]);
                fd[0] = malloc(sizeof(int *));
                fd[0][0] = NULL;
                pid1 = executer(alltree, tree->left, fd, 0, 1, 2);
                if ((pid1 != 0) && (tree->right == NULL))
                    waitpid(pid1, &status, 0);
                if (tree->right != NULL)
                    executer(alltree, tree->right, fd, 0, 1, 2);
                free(fd[0]);
                while (wait(&status) != -1) {
                }
                freedom(alltree);
                if (WIFEXITED(status))
                    exit (WEXITSTATUS(status));
                else
                    exit (128 + WTERMSIG(status));
            }
            return pid;
        }
        pid1 = executer(alltree, tree->left, fd, fd0, fd1, num);
        if (tree->right == NULL)
            return 0;
        return executer(alltree, tree->right, fd, fd0, fd1, num);
    }
    if (tree->key->token == CPIP) {
        if (tree->key->count > 0) {
            if ((pid = fork()) == 0) {
                if ((fd0 == -1) || (fd1 == -1)) {
                    n = 0;
                    while (fd[0][n] != NULL) {
                        close(fd[0][n][1]);
                        close(fd[0][n][0]);
                        free(fd[0][n]);
                        n++;
                    }
                    free(fd[0]);
                    freedom(alltree);
                    fprintf(stderr, "It is not avalable to open a file!\n");
                    exit (1);
                }
                if (fd0 != 0) {
                    dup2(fd0, 0);
                    close(fd0);
                }
                if (fd1 != 1) {
                    dup2(fd1, 1);
                    close(fd1);
                }
                n = 0;
                if ((num == 1) && (fd0 == 0)) {
                    while (fd[0][n] != NULL)
                        n++;
                    if (n > 0)
                        dup2(fd[0][n - 1][1], 1);
                }
                n = 0;
                if ((num == 0) && ((fd1 == 1) || (fd0 == 0))) {
                    while (fd[0][n] != NULL)
                        n++;
                    if ((n > 1) && (fd1 == 1))
                        dup2(fd[0][n - 2][1], 1);
                    if ((n >= 1) && (fd0 == 0))
                        dup2(fd[0][n - 1][0], 0);
                }
                n = 0;
                while (fd[0][n] != NULL) {
                    close(fd[0][n][1]);
                    close(fd[0][n][0]);
                    free(fd[0][n]);
                    n++;
                }
                free(fd[0]);
                fd[0] = malloc(2 * sizeof(int *));
                fd[0][0] = malloc(2 * sizeof(int));
                fd[0][1] = NULL;
                if (pipe(fd[0][0]) == -1)
                    exit(1);
                pid1 = executer(alltree, tree->left, fd, 0, 1, 1);
                executer(alltree, tree->right, fd, 0, 1, 0);
                close(fd[0][0][0]);
                close(fd[0][0][1]);
                free(fd[0][0]);
                free(fd[0]);
                while (wait(&status) != -1) {
                }
                freedom(alltree);
                if (WIFEXITED(status))
                    exit (WEXITSTATUS(status));
                else
                    exit (128 + WTERMSIG(status));
            }
            return pid;
        }
        n = 0;
        while (fd[0][n] != NULL)
            n++;
        n += 2;
        fd[0] = realloc(fd[0], n * sizeof(int *));
        fd[0][n - 2] = malloc(2 * sizeof(int));
        fd[0][n - 1] = NULL;
        if (pipe(fd[0][n - 2]) == -1)
            exit(1);
        pid1 = executer(alltree, tree->left, fd, fd0, fd1, 1);
        executer(alltree, tree->right, fd, fd0, fd1, 0);
        close(fd[0][n - 2][0]);
        close(fd[0][n - 2][1]);
        free(fd[0][n - 2]);
        fd[0][n - 2] = NULL;
        return 0;
    }
    if (tree->key->token == COMORFILE) {
        if ((pid = fork()) == 0) {
            if ((fd0 == -1) || (fd1 == -1)) {
                n = 0;
                while (fd[0][n] != NULL) {
                    close(fd[0][n][1]);
                    close(fd[0][n][0]);
                    free(fd[0][n]);
                    n++;
                }
                free(fd[0]);
                freedom(alltree);
                fprintf(stderr, "It is not avalable to open a file!\n");
                exit (1);
            }
            if (fd0 != 0) {
                dup2(fd0, 0);
                close(fd0);
            }
            if (fd1 != 1) {
                dup2(fd1, 1);
                close(fd1);
            }
            n = 0;
            if ((num == 1) && (fd1 == 1)) {
                while (fd[0][n] != NULL)
                    n++;
                if (n > 0)
                    dup2(fd[0][n - 1][1], 1);
            }
            n = 0;
            if ((num == 0) && ((fd1 == 1) || (fd0 == 0))) {
                while (fd[0][n] != NULL)
                    n++;
                if ((n > 1) && (fd1 == 1))
                    dup2(fd[0][n - 2][1], 1);
                if ((n >= 1) && (fd0 == 0))
                    dup2(fd[0][n - 1][0], 0);
            }
            n = 0;
            while (fd[0][n] != NULL) {
                close(fd[0][n][1]);
                close(fd[0][n][0]);
                free(fd[0][n]);
                n++;
            }
            free(fd[0]);
            execvp(tree->key->command[0], tree->key->command);
            freedom(alltree);
            exit(127);
        }
        return pid;
    }
    return 0;
}

void freedom(struct tree *commandtree) {
    if (commandtree->left != NULL)
        freedom(commandtree->left);
    if (commandtree->right != NULL)
        freedom(commandtree->right);
    int i = 0;
    if (commandtree != NULL) {
        if (commandtree->key->command != NULL) {
            while (commandtree->key->command[i] != NULL) {
                free(commandtree->key->command[i]);
                i++;
            }
            free(commandtree->key->command);
        }
        free(commandtree->key);
        free(commandtree);
    }
    return;
}

int main(void) {
    struct tree *commandtree;
    commandtree = parser();
    if (commandtree == NULL)
        exit (0);
    int **fd;
    fd = malloc(sizeof(int *));
    fd[0] = NULL;
    executer(commandtree, commandtree, &fd, 0, 1, 2);
    int status;
    while (wait(&status) != -1) {
    }
    int n = 0;
    while (fd[n] != NULL) {
        free(fd[n]);
        n++;
    }
    free(fd);
    freedom(commandtree);
    if (WIFEXITED(status))
        exit (WEXITSTATUS(status));
    else
        exit (128 + WTERMSIG(status));
}
