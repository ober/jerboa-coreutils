/* libcoreutils.c -- C helper functions for jerboa-coreutils
 *
 * Compile: gcc -shared -fPIC -o libcoreutils.so libcoreutils.c
 *
 * These functions are used by utilities that need FFI access to POSIX APIs
 * not directly available in Chez Scheme.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <unistd.h>
#include <pwd.h>
#include <grp.h>
#include <time.h>
#include <utime.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/ioctl.h>

/* ========== chmod/stat helpers ========== */

int coreutils_chmod(const char *path, int mode) {
    return chmod(path, (mode_t)mode);
}

int coreutils_lstat_mode(const char *path) {
    struct stat st;
    if (lstat(path, &st) < 0) return -1;
    return (int)(st.st_mode & 07777);
}

int coreutils_stat_isdir(const char *path) {
    struct stat st;
    if (stat(path, &st) < 0) return 0;
    return S_ISDIR(st.st_mode) ? 1 : 0;
}

/* ========== chown helpers ========== */

int coreutils_chown(const char *path, int uid, int gid) {
    return chown(path, (uid_t)uid, (gid_t)gid);
}

int coreutils_lchown(const char *path, int uid, int gid) {
    return lchown(path, (uid_t)uid, (gid_t)gid);
}

int coreutils_getpwnam_uid(const char *name) {
    struct passwd *pw = getpwnam(name);
    if (!pw) return -1;
    return (int)pw->pw_uid;
}

int coreutils_getgrnam_gid(const char *name) {
    struct group *gr = getgrnam(name);
    if (!gr) return -1;
    return (int)gr->gr_gid;
}

/* ========== stat helpers ========== */

static struct stat stat_result_buf;

int coreutils_stat_call(const char *path, int follow) {
    int rc = follow ? stat(path, &stat_result_buf) : lstat(path, &stat_result_buf);
    if (rc < 0) return -1;
    return 0;
}

long coreutils_stat_get(int idx) {
    switch (idx) {
        case 0:  return (long)stat_result_buf.st_dev;
        case 1:  return (long)stat_result_buf.st_ino;
        case 2:  return (long)stat_result_buf.st_mode;
        case 3:  return (long)stat_result_buf.st_nlink;
        case 4:  return (long)stat_result_buf.st_uid;
        case 5:  return (long)stat_result_buf.st_gid;
        case 6:  return (long)stat_result_buf.st_rdev;
        case 7:  return (long)stat_result_buf.st_size;
        case 8:  return (long)stat_result_buf.st_blksize;
        case 9:  return (long)stat_result_buf.st_blocks;
        case 10: return (long)stat_result_buf.st_atime;
        case 11: return (long)stat_result_buf.st_mtime;
        case 12: return (long)stat_result_buf.st_ctime;
        default: return 0;
    }
}

const char* coreutils_uid_to_name(int uid) {
    struct passwd *pw = getpwuid((uid_t)uid);
    if (!pw) return NULL;
    return pw->pw_name;
}

const char* coreutils_gid_to_name(int gid) {
    struct group *gr = getgrgid((gid_t)gid);
    if (!gr) return NULL;
    return gr->gr_name;
}

/* ========== du helper ========== */

long coreutils_du_stat(const char *path, int field) {
    struct stat st;
    if (lstat(path, &st) < 0) return -1;
    switch (field) {
        case 0: return S_ISDIR(st.st_mode) ? 1 : 0;
        case 1: return (long)st.st_size;
        case 2: return (long)st.st_blocks;
        case 3: return S_ISLNK(st.st_mode) ? 1 : 0;
        default: return -1;
    }
}

/* ========== df / statvfs helpers ========== */

static struct statvfs statvfs_result_buf;

int coreutils_statvfs(const char *path) {
    if (statvfs(path, &statvfs_result_buf) < 0) return -1;
    return 0;
}

unsigned long coreutils_statvfs_get(int idx) {
    switch (idx) {
        case 0: return (unsigned long)statvfs_result_buf.f_bsize;
        case 1: return (unsigned long)statvfs_result_buf.f_frsize;
        case 2: return (unsigned long)statvfs_result_buf.f_blocks;
        case 3: return (unsigned long)statvfs_result_buf.f_bfree;
        case 4: return (unsigned long)statvfs_result_buf.f_bavail;
        case 5: return (unsigned long)statvfs_result_buf.f_files;
        case 6: return (unsigned long)statvfs_result_buf.f_ffree;
        case 7: return (unsigned long)statvfs_result_buf.f_favail;
        case 8: return (unsigned long)statvfs_result_buf.f_flag;
        case 9: return (unsigned long)statvfs_result_buf.f_namemax;
        default: return 0;
    }
}

/* ========== test / access helpers ========== */

int coreutils_test_access(const char *path, int mode) {
    return access(path, mode) == 0 ? 1 : 0;
}

long coreutils_test_stat(const char *path, int field) {
    struct stat st;
    int rc;
    if (field == 3) {
        rc = lstat(path, &st);
    } else {
        rc = stat(path, &st);
    }
    if (rc < 0) return -1;
    switch (field) {
        case 0: return 1; /* exists */
        case 1: return S_ISREG(st.st_mode) ? 1 : 0;
        case 2: return S_ISDIR(st.st_mode) ? 1 : 0;
        case 3: return S_ISLNK(st.st_mode) ? 1 : 0;
        case 4: return (long)st.st_size;
        case 5: return S_ISBLK(st.st_mode) ? 1 : 0;
        case 6: return S_ISCHR(st.st_mode) ? 1 : 0;
        case 7: return S_ISFIFO(st.st_mode) ? 1 : 0;
        case 8: return S_ISSOCK(st.st_mode) ? 1 : 0;
        default: return -1;
    }
}

/* ========== ls helpers ========== */

static struct stat ls_stat_buf;

int coreutils_ls_lstat(const char *path, int follow) {
    if (follow)
        return stat(path, &ls_stat_buf);
    else
        return lstat(path, &ls_stat_buf);
}

long long coreutils_ls_stat_get(int field) {
    switch (field) {
        case 0: return (long long)ls_stat_buf.st_mode;
        case 1: return (long long)ls_stat_buf.st_nlink;
        case 2: return (long long)ls_stat_buf.st_uid;
        case 3: return (long long)ls_stat_buf.st_gid;
        case 4: return (long long)ls_stat_buf.st_size;
        case 5: return (long long)ls_stat_buf.st_atime;
        case 6: return (long long)ls_stat_buf.st_mtime;
        case 7: return (long long)ls_stat_buf.st_ctime;
        case 8: return (long long)ls_stat_buf.st_ino;
        case 9: return (long long)ls_stat_buf.st_blocks;
        default: return 0;
    }
}

static char ls_readlink_buf[4096];
const char* coreutils_ls_readlink(const char *path) {
    ssize_t n = readlink(path, ls_readlink_buf, sizeof(ls_readlink_buf) - 1);
    if (n < 0) return NULL;
    ls_readlink_buf[n] = '\0';
    return ls_readlink_buf;
}

int coreutils_isatty(int fd) {
    return isatty(fd);
}

int coreutils_terminal_width(int fd) {
    struct winsize ws;
    if (ioctl(fd, TIOCGWINSZ, &ws) < 0 || ws.ws_col == 0)
        return 80;
    return (int)ws.ws_col;
}

int coreutils_terminal_height(int fd) {
    struct winsize ws;
    if (ioctl(fd, TIOCGWINSZ, &ws) < 0 || ws.ws_row == 0)
        return 24;
    return (int)ws.ws_row;
}

/* Raw mode via tcgetattr/tcsetattr — no fork/exec needed */
#include <termios.h>
static struct termios coreutils_saved_termios;
static int coreutils_termios_saved = 0;

int coreutils_raw_mode_enter(int fd) {
    struct termios raw;
    if (tcgetattr(fd, &coreutils_saved_termios) < 0) return -1;
    coreutils_termios_saved = 1;
    raw = coreutils_saved_termios;
    raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
    raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
    raw.c_oflag &= ~(OPOST);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 0;
    return tcsetattr(fd, TCSAFLUSH, &raw);
}

int coreutils_raw_mode_exit(int fd) {
    if (!coreutils_termios_saved) return -1;
    coreutils_termios_saved = 0;
    return tcsetattr(fd, TCSAFLUSH, &coreutils_saved_termios);
}

static char ls_time_buf[64];
const char* coreutils_time_format(long t) {
    time_t tt = (time_t)t;
    time_t now = time(NULL);
    struct tm *tm = localtime(&tt);
    if (!tm) {
        snprintf(ls_time_buf, sizeof(ls_time_buf), "?");
        return ls_time_buf;
    }
    long diff = (long)(now - tt);
    if (diff < 0 || diff > 15552000) {
        strftime(ls_time_buf, sizeof(ls_time_buf), "%b %e  %Y", tm);
    } else {
        strftime(ls_time_buf, sizeof(ls_time_buf), "%b %e %H:%M", tm);
    }
    return ls_time_buf;
}

/* ========== cp/mv helpers ========== */

static struct stat cp_stat_buf;

int coreutils_cp_lstat(const char *path) {
    return lstat(path, &cp_stat_buf);
}

long coreutils_cp_stat_get(int field) {
    switch (field) {
        case 0: return (long)(cp_stat_buf.st_mode & 07777);
        case 1: return (long)cp_stat_buf.st_nlink;
        case 2: return (long)cp_stat_buf.st_uid;
        case 3: return (long)cp_stat_buf.st_gid;
        case 4: return (long)cp_stat_buf.st_size;
        case 5: return (long)cp_stat_buf.st_atime;
        case 6: return (long)cp_stat_buf.st_mtime;
        case 7:
            if (S_ISREG(cp_stat_buf.st_mode)) return 0;
            if (S_ISDIR(cp_stat_buf.st_mode)) return 1;
            if (S_ISLNK(cp_stat_buf.st_mode)) return 2;
            return 3;
        default: return 0;
    }
}

static char cp_readlink_buf[4096];
const char* coreutils_cp_readlink(const char *path) {
    ssize_t n = readlink(path, cp_readlink_buf, sizeof(cp_readlink_buf) - 1);
    if (n < 0) return NULL;
    cp_readlink_buf[n] = '\0';
    return cp_readlink_buf;
}

int coreutils_symlink(const char *target, const char *linkpath) {
    return symlink(target, linkpath);
}

int coreutils_link(const char *target, const char *linkpath) {
    return link(target, linkpath);
}

int coreutils_utime(const char *path, long atime, long mtime) {
    struct utimbuf ut;
    ut.actime = (time_t)atime;
    ut.modtime = (time_t)mtime;
    return utime(path, &ut);
}

int coreutils_mkdir(const char *path, int mode) {
    return mkdir(path, (mode_t)mode);
}

/* ========== rm helpers ========== */

int coreutils_lstat_type(const char *path) {
    struct stat st;
    if (lstat(path, &st) < 0) return -1;
    return S_ISDIR(st.st_mode) ? 1 : 0;
}

int coreutils_unlink(const char *path) {
    return unlink(path);
}

int coreutils_rmdir(const char *path) {
    return rmdir(path);
}

int coreutils_access_w(const char *path) {
    return access(path, W_OK);
}

/* ========== mv helpers ========== */

int coreutils_rename(const char *src, const char *dst) {
    return rename(src, dst);
}

int coreutils_stat_get_mode(const char *path) {
    struct stat st;
    if (stat(path, &st) < 0) return -1;
    return (int)(st.st_mode & 07777);
}

long coreutils_stat_atime(const char *path) {
    struct stat st;
    if (stat(path, &st) < 0) return -1;
    return (long)st.st_atime;
}

long coreutils_stat_mtime(const char *path) {
    struct stat st;
    if (stat(path, &st) < 0) return -1;
    return (long)st.st_mtime;
}

/* ========== shred helper ========== */

long long coreutils_file_size(const char *path) {
    struct stat st;
    if (stat(path, &st) < 0) return -1;
    return (long long)st.st_size;
}

int coreutils_fsync(int fd) {
    return fsync(fd);
}

/* ========== install helpers ========== */

int coreutils_chgrp_chown(const char *path, int gid) {
    return chown(path, (uid_t)-1, (gid_t)gid);
}

int coreutils_chgrp_lchown(const char *path, int gid) {
    return lchown(path, (uid_t)-1, (gid_t)gid);
}

/* ========== mktemp helper ========== */

/* Create a temporary file via mkstemp(3) with O_EXCL atomicity.
 * template must end with XXXXXX. Returns fd on success, -1 on error.
 * The template buffer is modified in-place with the actual filename. */
static char mkstemp_result_buf[4096];
int coreutils_mkstemp(const char *tmpl) {
    size_t len = strlen(tmpl);
    if (len >= sizeof(mkstemp_result_buf)) return -1;
    memcpy(mkstemp_result_buf, tmpl, len + 1);
    return mkstemp(mkstemp_result_buf);
}

/* Create a temporary directory via mkdtemp(3) with atomic creation.
 * template must end with XXXXXX. Returns 0 on success, -1 on error.
 * The template buffer is modified in-place with the actual dirname. */
int coreutils_mkdtemp(const char *tmpl) {
    size_t len = strlen(tmpl);
    if (len >= sizeof(mkstemp_result_buf)) return -1;
    memcpy(mkstemp_result_buf, tmpl, len + 1);
    return mkdtemp(mkstemp_result_buf) ? 0 : -1;
}

const char* coreutils_mkstemp_get_path(void) {
    return mkstemp_result_buf;
}

/* ========== readlink FFI helper ========== */

static char readlink_result_buf[4096];
const char* coreutils_readlink(const char *path) {
    ssize_t n = readlink(path, readlink_result_buf, sizeof(readlink_result_buf) - 1);
    if (n < 0) return NULL;
    readlink_result_buf[n] = '\0';
    return readlink_result_buf;
}

/* ========== realpath FFI helper ========== */

static char realpath_result_buf[4096];
const char* coreutils_realpath(const char *path) {
    char *result = realpath(path, realpath_result_buf);
    return result;  /* NULL on error */
}

/* ========== file size helper (for shred) ========== */

long long coreutils_stat_size(const char *path) {
    struct stat st;
    if (stat(path, &st) < 0) return -1;
    return (long long)st.st_size;
}

/* ========== fsync by path helper (for shred) ========== */

int coreutils_fsync_path(const char *path) {
    int fd = open(path, O_RDWR);
    if (fd < 0) return -1;
    int rc = fsync(fd);
    close(fd);
    return rc;
}
