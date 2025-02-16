:- module(encryption, [encrypt_file/3,
                       decrypt_file/3,
                       encrypt_files/2,
                       decrypt_files/2,
                       mnemos_aes_key/1]).

:- use_module(library(crypto)).

encrypt_file(InputFile, OutFile, Key) :-
    open(InputFile, read, InStream, [type(binary)]),
    read_string(InStream, _, In),
    crypto_n_random_bytes(16, IV),
    string_codes(IVStr, IV),
    crypto_data_encrypt(In, 'aes-256-cbc', Key, IV, CipherText, []),
    open(OutFile, write, OutStream, [type(binary)]),
    write(OutStream, IVStr),
    write(OutStream, CipherText),
    close(InStream),
    close(OutStream).

decrypt_file(InputFile, OutputFile, Key) :-
    open(InputFile, read, InStream, [type(binary)]),
    read_stream_to_codes(InStream, InBytes),
    length(IV, 16),
    append(IV, CipherBytes, InBytes),
    string_codes(CipherText, CipherBytes),
    crypto_data_decrypt(CipherText, 'aes-256-cbc', Key, IV, PlainText, []),
    open(OutputFile, write, OutStream, [type(binary)]),
    write(OutStream, PlainText),
    close(InStream),
    close(OutStream).

mnemos_aes_key(Key) :-
    getenv('MNEMOS_AES_KEY', KeyStr), string_codes(KeyStr, Key).

encrypt_files(Key, Xs) :-
    forall(member(FileName-EncryptedFileName, Xs), encrypt_file(FileName, EncryptedFileName, Key)).

decrypt_files(Key, Xs) :-
    forall(member(FileName-EncryptedFileName, Xs), decrypt_file(EncryptedFileName, FileName, Key)).


% ?- setenv('MNEMOS_AES_KEY', "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").

% ?- mnemos_aes_key(Key), encrypt_files(Key, ["../resources/supplements/coretest.pl"-"../resources/supplements/core.enc"]).

% ?- mnemos_aes_key(Key), decrypt_files(Key, ["../resources/supplements/coretest.pl"-"../resources/supplements/core.enc"]).
