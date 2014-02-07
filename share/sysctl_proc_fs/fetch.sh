#!/bin/bash

source='https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/plain/Documentation/sysctl'

for file in 'abi' 'fs' 'kernel' 'net' 'sunrpc' 'vm'; do
    wget --directory-prefix=documentation \
        "https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/plain/Documentation/sysctl/$file.txt"
done
