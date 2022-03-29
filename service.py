from flask import Flask

app = Flask(__name__)

@app.route('/')
def index():

    return 'fuck bitch!'

if __name__ == '__main__':
    app.run()

    # netstat -ntulp |grep 28082


